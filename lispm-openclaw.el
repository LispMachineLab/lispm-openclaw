;;; lispm-openclaw.el --- OpenClaw, one of the editor's agents  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-openclaw
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools, processes
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; OpenClaw (https://openclaw.ai), the open-source personal assistant
;; that answers wherever you chat with it, as one of the editor's agents
;; of LispM Agent (lispm-agents.el), beside Claude Code and Codex: a
;; package of its own, which LispM Desktop's App Store installs.  Loading
;; this file makes OpenClaw an agent (`lispm-agents-define'); unloading
;; it, as the App Store does when it removes it, takes it away again.  It
;; takes part while `openclaw' is on PATH: k in `lispm-agents-map' starts
;; it in the current project, and its sessions join the others.
;;
;; It needs lispm-agent 0.8.0 or later (the package `lispm'), which LispM
;; Desktop loads before it.  Package-Requires leaves it out on purpose:
;; package.el would not activate this package where lispm-agent is a
;; checkout on the load path rather than a package, or a package of an
;; older version, and it could not be loaded at all.
;;
;; OpenClaw keeps its conversations in its Gateway, a service on this
;; computer or on another, which every way of talking to OpenClaw, its
;; terminal UI, its web UI and its chat channels, shares.  So:
;;
;; - The sessions are asked of the Gateway in its own protocol, through
;;   `openclaw gateway call', which knows where the Gateway is and how to
;;   be let in: one elsewhere serves as well as one here.  Lists of the
;;   sessions have them asked in the background, again once OpenClaw's
;;   session store here changed, or `lispm-openclaw-refresh-interval'
;;   seconds after they last were.  The conversations listed are yours
;;   with OpenClaw, in the terminal, the web UI or a chat app; those of
;;   groups, of OpenClaw's subagents and of its scheduled jobs are left
;;   out.
;; - Starting OpenClaw in a project makes a session working in the
;;   project, and opens OpenClaw's own terminal UI, `openclaw tui', on
;;   it.  A Gateway that cannot work there, as an older one, or one
;;   elsewhere that lacks the project, makes a chat instead, and says so.
;; - Resuming a session opens the terminal UI on it.
;; - Renaming a session gives it a label, which OpenClaw keeps unique;
;;   deleting one has the Gateway delete it with its transcript.  A
;;   session's transcript is its conversation, in Markdown.
;;
;; While LispM runs, OpenClaw talks with the other agents, LispM's and
;; other programs' such as Claude Code (lispm-agents-bridge.el):
;;
;; - What another agent sends a session goes to the Gateway as its user's
;;   next message, after the run under way if there is one.  OpenClaw's
;;   terminal UI shows it.
;; - A Gateway on this computer gives the session LispM's tools: a server
;;   of the session's own, in the Gateway's MCP servers (`openclaw mcp
;;   set'), off but for that session.  It goes with the terminal.
;; - A Gateway elsewhere cannot reach Emacs, and an older one cannot give
;;   a session a server of its own: such a session gets no tools, and its
;;   final reply to a request answers it.
;;
;; OpenClaw sees nothing else of Emacs's state.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'project)
(require 'lispm-agents)
(require 'lispm-agents-mcp)

;; Since lispm-agent 0.8.0: servers serving nothing.
(declare-function lispm-agents-context-presence-make "lispm-agents-context")

(defgroup lispm-openclaw nil
  "OpenClaw, one of the editor's agents."
  :group 'tools
  :prefix "lispm-openclaw-")

(defcustom lispm-openclaw-agent nil
  "The OpenClaw agent new sessions are made for: its id, as `openclaw agents
list' gives it.  nil makes them for the Gateway's default agent."
  :type '(choice (const :tag "The Gateway's default agent" nil) string))

(defcustom lispm-openclaw-refresh-interval 30
  "Seconds after which a list of the sessions asks OpenClaw's Gateway for
them again.  It asks sooner once OpenClaw's session store on this
computer changed; a Gateway elsewhere tells of its changes only when
asked."
  :type 'number)

(defconst lispm-openclaw-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory of this package, holding its icon.")

(defconst lispm-openclaw-program "openclaw"
  "OpenClaw's command.")

(defconst lispm-openclaw-timeout 10
  "Seconds OpenClaw's Gateway has to answer a call.")

(defconst lispm-openclaw-refresh-pause 2
  "The fewest seconds between two askings of the Gateway for the sessions in
the background, however often the session store changes.")

(defconst lispm-openclaw-session-limit 100
  "The most sessions asked of the Gateway, the most recent.")

(defconst lispm-openclaw-history-limit 1000
  "The most messages of a conversation a transcript reads, the latest: as
many as the Gateway gives at once.")

(define-error 'lispm-openclaw-error "OpenClaw" 'user-error)
(define-error 'lispm-openclaw-unreachable "OpenClaw's Gateway cannot be reached"
              'lispm-openclaw-error)
(define-error 'lispm-openclaw-refused "OpenClaw" 'lispm-openclaw-error)

(defun lispm-openclaw-state-directory ()
  "Return OpenClaw's state directory: OPENCLAW_STATE_DIR, else ~/.openclaw."
  (let ((directory (getenv "OPENCLAW_STATE_DIR")))
    (file-name-as-directory
     (expand-file-name (if (and directory (not (string-empty-p directory))) directory "~/.openclaw")))))

;;;; The Gateway (pure)

(defun lispm-openclaw--json (text)
  "Return the JSON of TEXT: objects as plists, arrays as lists, and null
and false as nil."
  (json-parse-string text :object-type 'plist :array-type 'list
                     :null-object nil :false-object nil))

(defun lispm-openclaw--encode (params)
  "Return PARAMS, a plist, as JSON in a string a command line takes."
  (let ((json (json-serialize params)))
    (if (multibyte-string-p json) json (decode-coding-string json 'utf-8))))

(defun lispm-openclaw--arguments (method params)
  "Return the arguments of `openclaw' calling the Gateway's METHOD with
PARAMS, a plist, and printing its answer as JSON."
  (list "gateway" "call" method "--json"
        "--timeout" (number-to-string (* 1000 lispm-openclaw-timeout))
        "--params" (lispm-openclaw--encode params)))

(defun lispm-openclaw--first-json (output)
  "Return the JSON object OUTPUT begins a line with, or nil."
  (when-let* ((start (string-match "^{" output)))
    (with-temp-buffer
      (insert output)
      (goto-char (1+ start))
      (ignore-errors (json-parse-buffer :object-type 'plist :array-type 'list
                                        :null-object nil :false-object nil)))))

(defun lispm-openclaw--said (errors)
  "Return what OpenClaw said in ERRORS, what it wrote failing: its first
line, without what says only that a call failed."
  (let ((line (car (split-string errors "\n" t "[ \t]+"))))
    (if line
        (replace-regexp-in-string
         "\\`\\(?:\\[openclaw\\] \\)?\\(?:Failed to start CLI\\|Gateway call failed\\): \\(?:[A-Za-z]*Error: \\)?"
         "" line)
      "it said nothing")))

(defun lispm-openclaw--answer (output errors)
  "Return the answer of the Gateway in OUTPUT, what `openclaw gateway call
--json' printed, and ERRORS, what it wrote to its error output.
Signal `lispm-openclaw-unreachable' when the Gateway could not be
reached, and `lispm-openclaw-refused' when it answered with an error or
gave no answer, saying what OpenClaw said."
  (let ((json (lispm-openclaw--first-json output)))
    (cond ((and json (plist-get json :error) (not (eq (plist-get json :ok) t)))
           (let* ((failure (plist-get json :error))
                  (said (if (stringp failure)
                            failure
                          (or (plist-get failure :message) (format "%S" failure)))))
             (signal (if (and (consp failure) (equal (plist-get failure :type) "gateway_transport_error"))
                         'lispm-openclaw-unreachable
                       'lispm-openclaw-refused)
                     (list (car (split-string said "\n" t))))))
          (json json)
          (t
           ;; Older OpenClaws say only in words that the Gateway did not
           ;; answer.
           (let ((said (lispm-openclaw--said (concat errors "\n" output))))
             (signal (if (string-match-p "gateway \\(?:timeout\\|closed\\)\\|ECONNREFUSED\\|not reachable"
                                         said)
                         'lispm-openclaw-unreachable
                       'lispm-openclaw-refused)
                     (list said)))))))

;;;; The Gateway (effects)

(defun lispm-openclaw--call (method &optional params)
  "Call the Gateway's METHOD with PARAMS, a plist, and return its answer.
Signal `lispm-openclaw-unreachable' when the Gateway cannot be reached in
`lispm-openclaw-timeout' seconds, and `lispm-openclaw-refused' when it
refuses."
  (let ((errors (make-temp-file "lispm-openclaw"))
        (coding-system-for-read 'utf-8))
    (unwind-protect
        (with-temp-buffer
          (apply #'call-process lispm-openclaw-program nil (list t errors) nil
                 (lispm-openclaw--arguments method params))
          (lispm-openclaw--answer (buffer-string)
                                  (with-temp-buffer (insert-file-contents errors)
                                                    (buffer-string))))
      (delete-file errors))))

(defun lispm-openclaw--error-message (err)
  "Return what ERR, an error, says, as a person reads it."
  (if (and (memq 'lispm-openclaw-error (get (car err) 'error-conditions)) (stringp (cadr err)))
      (concat (get (car err) 'error-message) ": " (cadr err))
    (error-message-string err)))

(defun lispm-openclaw--ask (method &optional params)
  "Call the Gateway's METHOD with PARAMS, a plist, and return its answer,
as `lispm-openclaw--call' does, for a person: a Gateway that cannot be
reached, or refuses, is a user error saying so."
  (condition-case err
      (lispm-openclaw--call method params)
    (lispm-openclaw-error (user-error "%s" (lispm-openclaw--error-message err)))))

(defun lispm-openclaw--call-async (method params callback)
  "Call the Gateway's METHOD with PARAMS, a plist, in the background.
CALLBACK is called with the answer and nil, or with nil and the error
reading it signalled, as (SYMBOL . DATA), see `lispm-openclaw--call'.
Return the process calling."
  (let ((output (generate-new-buffer " *lispm-openclaw*" t))
        (errors (generate-new-buffer " *lispm-openclaw errors*" t)))
    (make-process
     :name "lispm-openclaw" :buffer output :stderr errors :noquery t
     :coding 'utf-8 :connection-type 'pipe
     :command (cons lispm-openclaw-program (lispm-openclaw--arguments method params))
     :sentinel
     (lambda (process _event)
       (unless (process-live-p process)
         ;; What it wrote last may not be in yet.
         (when-let* ((pipe (get-buffer-process errors)))
           (while (accept-process-output pipe 0.05 nil t))
           (delete-process pipe))
         (let ((result (condition-case err
                           (cons (lispm-openclaw--answer
                                  (with-current-buffer output (buffer-string))
                                  (with-current-buffer errors (buffer-string)))
                                 nil)
                         (error (cons nil err)))))
           (kill-buffer output)
           (kill-buffer errors)
           (funcall callback (car result) (cdr result))))))))

;;;; Sessions (pure)

(defun lispm-openclaw--time (milliseconds)
  "Return MILLISECONDS since the epoch, as OpenClaw keeps times, as float
seconds, or nil."
  (and (numberp milliseconds) (/ milliseconds 1000.0)))

(defun lispm-openclaw--key-rest (key)
  "Return KEY without the agent it begins with: agent:main:lispm:a1 is
lispm:a1."
  (if (string-match "\\`agent:[^:]+:\\(.+\\)" key) (match-string 1 key) key))

(defun lispm-openclaw--short-id (key)
  "Return the end of KEY that tells its session from others: its last part,
eight characters at most, as of a UUID."
  (let ((last (car (last (split-string key ":")))))
    (if (> (length last) 8) (substring last 0 8) last)))

(defun lispm-openclaw--line (text)
  "Return the first line of TEXT with its spaces made one, or nil when it
has none."
  (when-let* (((stringp text))
              (line (car (split-string text "\n" t "[ \t]+"))))
    (replace-regexp-in-string "[ \t]+" " " line)))

(defun lispm-openclaw--name (row field)
  "Return the name in FIELD of ROW, as the Gateway lists a session, or nil
when it names no conversation: older Gateways name a session after the
program first talking in it, as openclaw-tui, and derive titles from
the metadata they put before what you wrote."
  (when-let* ((name (lispm-openclaw--line (plist-get row field))))
    (unless (or (string-match-p "\\`\\(?:openclaw-[a-z-]+\\|cli\\|webchat\\(?:-ui\\)?\\)\\'" name)
                (string-match-p "(untrusted metadata)" name))
      name)))

(defun lispm-openclaw--title (row)
  "Return the title of the session of ROW, as the Gateway lists it: its
label, else the name OpenClaw gave it, else the beginning of its last
message, else its key."
  (or (seq-some (lambda (field) (lispm-openclaw--name row field))
                '(:label :displayName :derivedTitle :subject :lastMessagePreview))
      (lispm-openclaw--key-rest (plist-get row :key))))

(defun lispm-openclaw--conversation-p (row)
  "Return non-nil when ROW, as the Gateway lists it, is a conversation of
yours with OpenClaw: one of its agents', not of a group, nor of one of
its subagents or scheduled jobs, nor in the background; and one that
was named or said something."
  (let ((key (plist-get row :key))
        (depth (plist-get row :spawnDepth)))
    (and (stringp key)
         (string-prefix-p "agent:" key)
         (not (string-match-p ":\\(?:subagent\\|cron\\|hook\\):" key))
         (equal (or (plist-get row :kind) "direct") "direct")
         (not (plist-get row :isBackground))
         (not (plist-get row :spawnedBy))
         (not (plist-get row :archived))
         (or (not (numberp depth)) (zerop depth))
         (or (plist-get row :lastInteractionAt)
             (seq-some (lambda (field) (lispm-openclaw--name row field))
                       '(:label :displayName :derivedTitle :subject))))))

(defun lispm-openclaw--session (row)
  "Return the session of ROW, as the Gateway lists it, or nil when it is
none of your conversations, see `lispm-openclaw--conversation-p'.  Its
id is its key; its directory the one it was started in, when that is
here, else home, which makes it a chat; its time when it was last
talked to."
  (when (lispm-openclaw--conversation-p row)
    (let ((directory (plist-get row :spawnedCwd)))
      (list :agent 'openclaw
            :id (plist-get row :key)
            :cwd (if (and (stringp directory) (file-directory-p directory))
                     (directory-file-name (expand-file-name directory))
                   (expand-file-name "~"))
            :title (lispm-openclaw--title row)
            :time (or (seq-some (lambda (field) (lispm-openclaw--time (plist-get row field)))
                                '(:lastInteractionAt :lastActivityAt :updatedAt :createdAt))
                      (float-time))))))

(defun lispm-openclaw--sessions-of (answer)
  "Return the sessions of ANSWER, the Gateway's answer listing them."
  (delq nil (mapcar #'lispm-openclaw--session (plist-get answer :sessions))))

(defun lispm-openclaw--list-params ()
  "Return the parameters asking the Gateway for the sessions: the most
recent, with the names and last messages that title them.  They are
those every Gateway since early 2026 takes."
  (list :limit lispm-openclaw-session-limit :includeDerivedTitles t :includeLastMessage t))

(defun lispm-openclaw--fresh-p (cache stamp now)
  "Return non-nil when CACHE, as `lispm-openclaw--cache' holds it, still
holds at NOW, float seconds: OpenClaw's session store here is as STAMP
says, as it was then, and `lispm-openclaw-refresh-interval' has not
passed since."
  (and cache
       (equal (plist-get cache :stamp) stamp)
       (< (- now (plist-get cache :time)) lispm-openclaw-refresh-interval)))

;;;; Sessions (effects)

(defvar lispm-openclaw--cache nil
  "The sessions last asked of the Gateway, as (:stamp STAMP :time TIME
:sessions SESSIONS): STAMP is `lispm-openclaw--stamp' as it was then,
TIME when, float seconds.")

(defvar lispm-openclaw--refreshing nil
  "The process asking the Gateway for the sessions in the background, or nil.")

(defvar lispm-openclaw--refreshed-at nil
  "When the Gateway was last asked for the sessions in the background,
float seconds, or nil.")

(defvar lispm-openclaw--unavailable nil
  "What was said last of the sessions being unavailable, said once.")

(defun lispm-openclaw--store-files ()
  "Return the files of OpenClaw's session store here: every agent's
database and its write-ahead log, or, of an older OpenClaw, its
sessions.json."
  (let ((agents (expand-file-name "agents" (lispm-openclaw-state-directory))))
    (sort (append (file-expand-wildcards (expand-file-name "*/agent/*.sqlite*" agents))
                  (file-expand-wildcards (expand-file-name "*/sessions/sessions.json" agents)))
          #'string<)))

(defun lispm-openclaw--stamp ()
  "Return the files of OpenClaw's session store here with their
modification times, which every change of a session there updates."
  (mapcar (lambda (file) (cons file (lispm-agents-file-time file)))
          (lispm-openclaw--store-files)))

(defun lispm-openclaw--remember (stamp sessions)
  "Remember SESSIONS as the Gateway listed them now, its store as STAMP
says."
  (setq lispm-openclaw--cache (list :stamp stamp :time (float-time) :sessions sessions)))

(defun lispm-openclaw--unavailable (err)
  "Say that the sessions are unavailable, as ERR, an error, says, unless
that was said last."
  (let ((said (lispm-openclaw--error-message err)))
    (unless (equal said lispm-openclaw--unavailable)
      (setq lispm-openclaw--unavailable said)
      (message "OpenClaw sessions unavailable: %s" said))))

(defun lispm-openclaw--listed (answer err)
  "Return the sessions of ANSWER, the Gateway's listing, or none when it
could not list them, which ERR, the error, says."
  (if err
      (progn (lispm-openclaw--unavailable err) nil)
    (setq lispm-openclaw--unavailable nil)
    (lispm-openclaw--sessions-of answer)))

(defun lispm-openclaw-sessions ()
  "Return a session for every conversation of yours with OpenClaw, asking
its Gateway again when the sessions known may be stale, see
`lispm-openclaw--fresh-p'.  A Gateway that cannot be asked lists none,
and says so."
  (let ((stamp (lispm-openclaw--stamp)))
    (unless (lispm-openclaw--fresh-p lispm-openclaw--cache stamp (float-time))
      (lispm-openclaw--remember
       stamp (condition-case err
                 (lispm-openclaw--listed
                  (lispm-openclaw--call "sessions.list" (lispm-openclaw--list-params)) nil)
               (error (lispm-openclaw--listed nil err)))))
    (plist-get lispm-openclaw--cache :sessions)))

(defun lispm-openclaw--refresh (stamp)
  "Ask the Gateway for the sessions in the background, OpenClaw's store
here as STAMP says, unless it is being asked, or was a moment ago, see
`lispm-openclaw-refresh-pause'.  Lists of the sessions are told when
they changed."
  (unless (or (process-live-p lispm-openclaw--refreshing)
              (and lispm-openclaw--refreshed-at
                   (< (- (float-time) lispm-openclaw--refreshed-at) lispm-openclaw-refresh-pause)))
    (setq lispm-openclaw--refreshed-at (float-time)
          lispm-openclaw--refreshing
          (lispm-openclaw--call-async
           "sessions.list" (lispm-openclaw--list-params)
           (lambda (answer err)
             (let ((known lispm-openclaw--cache)
                   (sessions (lispm-openclaw--listed answer err)))
               (lispm-openclaw--remember stamp sessions)
               (unless (and known (equal (plist-get known :sessions) sessions))
                 (lispm-agents-sessions-arrived))))))))

(defun lispm-openclaw--read-sessions ()
  "Return the sessions known now, never waiting: the reader of OpenClaw's
sessions for lists of them, see `lispm-agents-add-session-reader'.
When they may be stale, the Gateway is asked again in the background;
before it ever answered, they are `pending'."
  (let ((stamp (lispm-openclaw--stamp)))
    (unless (lispm-openclaw--fresh-p lispm-openclaw--cache stamp (float-time))
      (lispm-openclaw--refresh stamp))
    (if lispm-openclaw--cache (plist-get lispm-openclaw--cache :sessions) 'pending)))

(defun lispm-openclaw--reread ()
  "Have the sessions asked of the Gateway again, those known shown
meanwhile.  This is on `lispm-agents-reread-sessions-hook'."
  (when lispm-openclaw--cache
    (setq lispm-openclaw--cache (plist-put (copy-sequence lispm-openclaw--cache) :time 0)))
  (setq lispm-openclaw--refreshed-at nil))

(defun lispm-openclaw--changed (key &optional title)
  "Show the session KEY as renamed to TITLE, or, with TITLE nil, as
deleted, until the Gateway is asked again, which is at once."
  (when lispm-openclaw--cache
    (setq lispm-openclaw--cache
          (plist-put (copy-sequence lispm-openclaw--cache) :sessions
                     (delq nil (mapcar (lambda (session)
                                         (cond ((not (equal (plist-get session :id) key)) session)
                                               (title (plist-put (copy-sequence session) :title title))))
                                       (plist-get lispm-openclaw--cache :sessions))))))
  (lispm-openclaw--reread))

(defun lispm-openclaw--rename (session title)
  "Rename SESSION to TITLE: its label, which OpenClaw refuses when another
session has it."
  (let ((key (plist-get session :id)))
    (lispm-openclaw--ask "sessions.patch" (list :key key :label title))
    (lispm-openclaw--changed key title)))

(defun lispm-openclaw--delete (session)
  "Delete SESSION in the Gateway, with its transcript, of which OpenClaw
may keep an archive.  OpenClaw refuses to delete an agent's main
session."
  (let ((key (plist-get session :id)))
    (lispm-openclaw--ask "sessions.delete" (list :key key :deleteTranscript t))
    (lispm-openclaw--changed key)))

;;;; Transcripts

(defun lispm-openclaw--message-text (message)
  "Return the text of MESSAGE, as the Gateway gives a conversation's
messages, or nil: what was written, without tools called or thoughts."
  (let* ((content (plist-get message :content))
         (text (cond ((stringp content) content)
                     ((listp content)
                      (string-join (delq nil (mapcar (lambda (part)
                                                       (and (equal (plist-get part :type) "text")
                                                            (plist-get part :text)))
                                                     content))
                                   "\n\n")))))
    (and text (not (string-blank-p text)) (string-trim text))))

(defun lispm-openclaw--markdown (title messages)
  "Return the transcript of the session TITLE, in Markdown: what you wrote
and what OpenClaw answered, of MESSAGES, oldest first, as the Gateway
gives them."
  (concat
   "# " title "\n"
   (mapconcat
    (lambda (message)
      (when-let* ((who (pcase (plist-get message :role)
                         ("user" "You")
                         ("assistant" "OpenClaw")))
                  (text (lispm-openclaw--message-text message)))
        (format "\n## %s%s\n\n%s\n"
                who
                (if-let* (((equal who "You"))
                          (time (lispm-openclaw--time (plist-get message :timestamp))))
                    (format-time-string " · %F %R" time)
                  "")
                text)))
    messages "")))

(defun lispm-openclaw--transcript (session)
  "Return a file holding the transcript of SESSION, in Markdown, written to
a fresh temporary file.  It holds the latest
`lispm-openclaw-history-limit' messages at most."
  (let* ((answer (lispm-openclaw--ask "chat.history" (list :sessionKey (plist-get session :id)
                                                            :limit lispm-openclaw-history-limit)))
         (file (make-temp-file (format "openclaw-%s-" (lispm-openclaw--short-id (plist-get session :id)))
                               nil ".md"))
         (coding-system-for-write 'utf-8-unix))
    (write-region (lispm-openclaw--markdown (plist-get session :title) (plist-get answer :messages))
                  nil file nil 'silent)
    file))

;;;; Starting and resuming

(defun lispm-openclaw--uuid ()
  "Return a random UUID, of version 4."
  (format "%04x%04x-%04x-4%03x-%04x-%04x%04x%04x"
          (random 65536) (random 65536) (random 65536) (random 4096)
          (logior #x8000 (random 16384)) (random 65536) (random 65536) (random 65536)))

(defun lispm-openclaw--new-key ()
  "Return the key of a new session, for `lispm-openclaw-agent', else for
the Gateway's default agent."
  (let ((id (concat "lispm:" (lispm-openclaw--uuid))))
    (if lispm-openclaw-agent (format "agent:%s:%s" lispm-openclaw-agent id) id)))

(defun lispm-openclaw--project ()
  "Return the root of the project of the main window's buffer, or nil.
New sessions start there, see `lispm-agents-new'."
  (with-selected-window (funcall lispm-agents-main-window-function)
    (when-let* ((project (project-current)))
      (directory-file-name (expand-file-name (project-root project))))))

(defun lispm-openclaw--create (project)
  "Have the Gateway make a session working in PROJECT, or a chat when it is
nil, and return its key.  A Gateway refusing to work in PROJECT makes a
chat, which is said."
  (let ((key (lispm-openclaw--new-key)))
    (or (plist-get
         (condition-case err
             (lispm-openclaw--call "sessions.create"
                                   (append (list :key key) (and project (list :cwd project))))
           (lispm-openclaw-refused
            (unless project
              (signal (car err) (cdr err)))
            (message "OpenClaw cannot work in %s, so this is a chat: %s"
                     (abbreviate-file-name project) (cadr err))
            (lispm-openclaw--ask "sessions.create" (list :key key)))
           (lispm-openclaw-unreachable
            (user-error "OpenClaw's Gateway cannot be reached (%s): %s"
                        (cadr err)
                        "`openclaw gateway start' starts it, M-x lispm-openclaw-setup sets OpenClaw up")))
         :key)
        key)))

(defun lispm-openclaw--new-arguments ()
  "Make a session in the project new sessions start in, and return (KEY .
ARGUMENTS) opening OpenClaw's terminal UI on it.  Outside projects it is
a chat."
  (let ((key (lispm-openclaw--create (lispm-openclaw--project))))
    (lispm-openclaw--reread)
    (cons key (list "tui" "--session" key))))

(defun lispm-openclaw--resume-arguments (key)
  "Return the arguments opening OpenClaw's terminal UI on the session KEY."
  (list "tui" "--session" key))

;;;; Other agents' messages, and LispM's tools

(defconst lispm-openclaw-answer-interval 3
  "Seconds between looks at whether a session answered a request.")

(defconst lispm-openclaw-answer-timeout (* 6 3600)
  "Seconds a session has to answer a request.")

(cl-defstruct (lispm-openclaw-mcp (:include lispm-agents-mcp)
                                  (:constructor lispm-openclaw-mcp--make))
  "The server of an OpenClaw terminal, which its session reaches as one of
the Gateway's MCP servers.")

(defvar lispm-openclaw--served nil
  "Live servers of OpenClaw terminals, as (SERVER NAME KEY): NAME is the
server's in the Gateway's config, KEY the session's.")

(defvar lispm-openclaw--swept nil
  "Non-nil once servers an Emacs no longer running left in the Gateway's
config were taken away.")

(defun lispm-openclaw--messaging-p ()
  "Return non-nil when LispM serves the editor's agents its tools and
messages, and LispM Agent can make a peer of an agent it serves none."
  (and (featurep 'lispm-agents-bridge) (fboundp 'lispm-agents-context-presence-make)))

(defun lispm-openclaw--command (&rest args)
  "Run OpenClaw with ARGS; return its output, or signal
`lispm-openclaw-refused' with what it said when it fails."
  (let ((errors (make-temp-file "lispm-openclaw"))
        (coding-system-for-read 'utf-8))
    (unwind-protect
        (with-temp-buffer
          (let ((status (apply #'call-process lispm-openclaw-program nil (list t errors) nil args)))
            (unless (eq status 0)
              (signal 'lispm-openclaw-refused
                      (list (lispm-openclaw--said (concat (with-temp-buffer (insert-file-contents errors)
                                                                            (buffer-string))
                                                          "\n" (buffer-string))))))
            (buffer-string)))
      (delete-file errors))))

(defun lispm-openclaw--local-p ()
  "Return non-nil when OpenClaw's Gateway runs on this computer, as its
config here says, and so can reach Emacs."
  (equal (string-trim (or (ignore-errors (lispm-openclaw--command "config" "get" "gateway.mode")) ""))
         "local"))

(defun lispm-openclaw--server-name (server)
  "Return the name SERVER has among the Gateway's MCP servers."
  (concat "lispm_" (substring (md5 (lispm-agents-mcp-token server)) 0 10)))

(defun lispm-openclaw--server-config (server)
  "Return the Gateway's config of SERVER, off unless a session turns it on."
  (list :url (format "http://127.0.0.1:%d/mcp" (lispm-agents-mcp-port server))
        :transport "streamable-http"
        :headers (list :Authorization (concat "Bearer " (lispm-agents-mcp-token server)))
        :enabled :false))

(defun lispm-openclaw--sweep ()
  "Take from the Gateway's config the servers of terminals no Emacs runs
any more, as one that stopped without taking its own away leaves them;
once in an Emacs."
  (unless lispm-openclaw--swept
    (setq lispm-openclaw--swept t)
    (when-let* ((servers (ignore-errors
                           (lispm-openclaw--json (lispm-openclaw--command "mcp" "list" "--json")))))
      (cl-loop for (field _) on servers by #'cddr
               for name = (substring (symbol-name field) 1)
               when (and (string-prefix-p "lispm_" name)
                         (not (seq-find (lambda (entry) (equal (nth 1 entry) name)) lispm-openclaw--served)))
               do (ignore-errors (lispm-openclaw--command "mcp" "unset" name))))))

(defun lispm-openclaw--serve (directory key)
  "Serve LispM's tools to the session KEY, opened in a terminal in
DIRECTORY: a server of its own among the Gateway's MCP servers, on for
KEY alone.  Return it, or nil when the Gateway runs elsewhere or cannot
turn a server on for one session, as an older one."
  (when (lispm-openclaw--local-p)
    (lispm-openclaw--sweep)
    (let* ((server (lispm-agents-mcp-start "openclaw-mcp" #'lispm-openclaw-mcp--make directory))
           (name (lispm-openclaw--server-name server)))
      (condition-case err
          (progn
            (lispm-openclaw--command "mcp" "set" name
                                     (lispm-openclaw--encode (lispm-openclaw--server-config server)))
            (lispm-openclaw--call "sessions.patch"
                                  (list :key key
                                        :toolOverrides (list :mcpServers (list (intern (concat ":" name)) t))))
            (push (list server name key) lispm-openclaw--served)
            server)
        (error
         (ignore-errors (lispm-openclaw--command "mcp" "unset" name))
         (lispm-agents-context-stop server)
         (message "OpenClaw's session gets no tools of LispM's: %s" (lispm-openclaw--error-message err))
         nil)))))

(cl-defmethod lispm-agents-context-stop ((server lispm-openclaw-mcp))
  "Stop SERVER, and take it from the Gateway's config, in the background."
  (when-let* ((entry (assq server lispm-openclaw--served)))
    (setq lispm-openclaw--served (delq entry lispm-openclaw--served))
    (make-process :name "lispm-openclaw unset" :noquery t :connection-type 'pipe
                  :buffer nil :sentinel #'ignore
                  :command (list lispm-openclaw-program "mcp" "unset" (nth 1 entry))))
  (cl-call-next-method))

(defun lispm-openclaw--context (directory &optional key)
  "Make the session KEY, opened in a terminal in DIRECTORY, a peer of the
other agents while LispM runs: with LispM's tools when the Gateway can
give them, else with a server serving nothing."
  (when (and key (lispm-openclaw--messaging-p))
    (or (lispm-openclaw--serve directory key)
        (lispm-agents-context-presence-make))))

(defun lispm-openclaw--terminal-key (terminal)
  "Return the key of the session of TERMINAL; an error if unknown."
  (or (lispm-agents-terminal-session-id terminal)
      (error "%s runs no OpenClaw session known yet" (buffer-name terminal))))

(defun lispm-openclaw--send (key text callback)
  "Give the session KEY TEXT as its user's next message, in the background:
after its run under way, if any.  CALLBACK is called with the message's
idempotency key and nil, or with nil and the error, as (SYMBOL . DATA)."
  (let* ((id (lispm-openclaw--uuid))
         (params (list :sessionKey key :message text :idempotencyKey id)))
    (lispm-openclaw--call-async
     "chat.send" (append params (list :queueMode "followup"))
     (lambda (answer err)
       (if (and err (string-search "queueMode" (format "%s" (cdr err))))
           ;; An older Gateway knows no queueMode: it keeps a message for
           ;; after the run under way by itself.
           (lispm-openclaw--call-async "chat.send" params
                                       (lambda (answer err) (funcall callback (and answer id) err)))
         (funcall callback (and answer id) err))))))

(defun lispm-openclaw--deliver (terminal text)
  "Give the session of TERMINAL TEXT as its user's next message, which the
Gateway keeps until the session is free.  Return t; should the Gateway
refuse it, that is said."
  (lispm-openclaw--send (lispm-openclaw--terminal-key terminal) text
                        (lambda (_id err)
                          (when err
                            (message "OpenClaw did not take a message of another agent's: %s"
                                     (lispm-openclaw--error-message err)))))
  t)

(defun lispm-openclaw--sent-p (message id text)
  "Return non-nil when MESSAGE, as the Gateway gives a conversation's, is
the user's message sent with the idempotency key ID, or, of a Gateway
keeping no keys, the user's message TEXT."
  (and (equal (plist-get message :role) "user")
       (or (equal (plist-get message :idempotencyKey) (concat id ":user"))
           (equal (plist-get (plist-get message :__openclaw) :idempotencyKey) (concat id ":user"))
           (equal (lispm-openclaw--message-text message) (string-trim text)))))

(defun lispm-openclaw--reply-of (answer id text)
  "Return the final reply, in ANSWER, the Gateway's chat.history, to the
message TEXT sent with the idempotency key ID, or nil while there is
none: the first answer after it that ends a run, as one calling no tools
does."
  (when-let* ((after (cdr (seq-drop-while (lambda (message) (not (lispm-openclaw--sent-p message id text)))
                                          (plist-get answer :messages))))
              (final (seq-find (lambda (message)
                                 (and (equal (plist-get message :role) "assistant")
                                      (member (plist-get message :stopReason) '("stop" "end_turn" "length"))
                                      (lispm-openclaw--message-text message)))
                               after)))
    (lispm-openclaw--message-text final)))

(defun lispm-openclaw--await-reply (terminal key id text callback deadline)
  "Look, every `lispm-openclaw-answer-interval' seconds until DEADLINE,
whether the session KEY of TERMINAL has given its final reply to TEXT,
sent with the idempotency key ID; call CALLBACK with it once it has.
Looking stops with the terminal, whose end settles what it was asked."
  (cond ((not (lispm-agents-terminal-live-p terminal)))
        ((> (float-time) deadline)
         (funcall callback (format "OpenClaw gave no reply in %d hours." (/ lispm-openclaw-answer-timeout 3600))))
        (t
         (run-at-time
          lispm-openclaw-answer-interval nil
          (lambda ()
            (lispm-openclaw--call-async
             "chat.history" (list :sessionKey key :limit 100)
             (lambda (answer _err)
               (if-let* ((reply (and answer (lispm-openclaw--reply-of answer id text))))
                   (funcall callback reply)
                 (lispm-openclaw--await-reply terminal key id text callback deadline)))))))))

(defun lispm-openclaw--answer-request (terminal text callback)
  "Give the session of TERMINAL TEXT, a request, as its user's next
message, and call CALLBACK with its final reply to it, which answers the
request: for a session served no tools to reply with."
  (let ((key (lispm-openclaw--terminal-key terminal)))
    (lispm-openclaw--send
     key text
     (lambda (id err)
       (if err
           (funcall callback (format "OpenClaw did not take the request: %s"
                                     (lispm-openclaw--error-message err)))
         (lispm-openclaw--await-reply terminal key id text callback
                                      (+ (float-time) lispm-openclaw-answer-timeout)))))))

(defun lispm-openclaw-setup ()
  "Set OpenClaw up, in a terminal: `openclaw onboard' asks for a model
provider and its credentials, and sets the Gateway up as a service of
this computer, or names one elsewhere."
  (interactive)
  (unless (executable-find lispm-openclaw-program)
    (user-error "OpenClaw is not installed: `%s' is not on PATH" lispm-openclaw-program))
  (pop-to-buffer
   (let ((process-environment (lispm-agents-independent-environment process-environment)))
     (lispm-agents-terminal-make "openclaw onboard" lispm-openclaw-program "onboard"))))

;;;; The agent

(defun lispm-openclaw-new ()
  "Start a new OpenClaw session at the current project root."
  (interactive)
  (lispm-agents-new 'openclaw))

(lispm-agents-define
 'openclaw
 :name "OpenClaw"
 ;; OpenClaw's lobster, from its web UI.
 :icon `(lispm-agents-image-icon ,(expand-file-name "icons/openclaw.png" lispm-openclaw-directory) nil)
 ;; A claw.
 :text-icon "⋔"
 :command #'lispm-openclaw-new
 :key "k"
 :program lispm-openclaw-program
 :sessions #'lispm-openclaw-sessions
 :rename #'lispm-openclaw--rename
 :delete #'lispm-openclaw--delete
 :transcript #'lispm-openclaw--transcript
 :short-id #'lispm-openclaw--short-id
 :new-arguments #'lispm-openclaw--new-arguments
 :resume-arguments #'lispm-openclaw--resume-arguments
 :context #'lispm-openclaw--context
 :deliver #'lispm-openclaw--deliver
 :answer #'lispm-openclaw--answer-request)

(lispm-agents-add-session-reader 'openclaw #'lispm-openclaw--read-sessions)
(add-hook 'lispm-agents-reread-sessions-hook #'lispm-openclaw--reread)

(defun lispm-openclaw-unload-function ()
  "Take OpenClaw away from the editor's agents, as this package is unloaded."
  (remove-hook 'lispm-agents-reread-sessions-hook #'lispm-openclaw--reread)
  (lispm-agents-remove-session-reader 'openclaw)
  (when (process-live-p lispm-openclaw--refreshing)
    (delete-process lispm-openclaw--refreshing))
  (lispm-agents-undefine 'openclaw)
  ;; Unloading goes on as usual.
  nil)

(provide 'lispm-openclaw)
;;; lispm-openclaw.el ends here
