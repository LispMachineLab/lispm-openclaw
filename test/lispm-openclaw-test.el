;;; lispm-openclaw-test.el --- Tests for lispm-openclaw  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-openclaw
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

;; Run with `make test', lispm-agent's lisp/ on the load path.  They need
;; no OpenClaw: its command is test/fake-openclaw, which answers each of
;; the Gateway's methods as a test tells it to, and its state directory
;; a fresh one.

;;; Code:

(require 'ert)
(require 'lispm-openclaw)

(defconst lispm-openclaw-test-fake
  (expand-file-name "fake-openclaw" (file-name-directory (or load-file-name buffer-file-name)))
  "The stand-in for OpenClaw's command.")

;;;; A Gateway

(defun lispm-openclaw-test--json (object)
  "Return OBJECT as JSON, a string: plists are objects, vectors arrays."
  (lispm-openclaw--encode object))

(defun lispm-openclaw-test--answer (directory method answer)
  "Have test/fake-openclaw, keeping its files in DIRECTORY, answer the
Gateway's METHOD as ANSWER says: the JSON it prints, a plist or a
string, or (:json JSON :err ERRORS :status STATUS)."
  (let ((answer (if (and (consp answer) (memq (car answer) '(:json :err :status)))
                    answer
                  (list :json answer))))
    (cl-flet ((put (suffix text)
                (when text
                  (with-temp-file (expand-file-name (concat method suffix) directory)
                    (insert text)))))
      (put ".json" (let ((json (plist-get answer :json)))
                     (if (stringp json) json (and json (lispm-openclaw-test--json json)))))
      (put ".err" (plist-get answer :err))
      (put ".status" (and (plist-get answer :status) (number-to-string (plist-get answer :status)))))))

(defmacro lispm-openclaw-test-with-gateway (answers &rest body)
  "Run BODY with test/fake-openclaw as OpenClaw's command, answering the
Gateway's methods as ANSWERS says, an alist of (METHOD . ANSWER), see
`lispm-openclaw-test--answer', and OpenClaw's state directory a fresh
one, which `state' names.  `calls' returns the arguments of its calls, a
string each, and `params' the parameters of its calls of a method, as
plists.  No session is known at first."
  (declare (indent 1))
  `(let* ((fake-directory (make-temp-file "lispm-openclaw-test-fake" t))
          (state (make-temp-file "lispm-openclaw-test-state" t))
          (process-environment (append (list (concat "FAKE_OPENCLAW_DIR=" fake-directory)
                                             (concat "OPENCLAW_STATE_DIR=" state))
                                       process-environment))
          (lispm-openclaw-program lispm-openclaw-test-fake)
          (lispm-openclaw--cache nil)
          (lispm-openclaw--refreshing nil)
          (lispm-openclaw--refreshed-at nil)
          (lispm-openclaw--unavailable nil))
     (pcase-dolist (`(,method . ,answer) ,answers)
       (lispm-openclaw-test--answer fake-directory method answer))
     (cl-flet* ((calls ()
                  (let ((file (expand-file-name "calls" fake-directory)))
                    (and (file-exists-p file)
                         (with-temp-buffer (insert-file-contents file)
                                           (split-string (buffer-string) "\n" t)))))
                (params (method)
                  (delq nil (mapcar (lambda (call)
                                      (when (string-match (concat "\\`gateway call " (regexp-quote method)
                                                                  " .*--params \\(.*\\)\\'")
                                                          call)
                                        (lispm-openclaw--json (match-string 1 call))))
                                    (calls)))))
       (ignore #'params)
       (unwind-protect
           (progn ,@body)
         (delete-directory fake-directory t)
         (delete-directory state t)))))

(defun lispm-openclaw-test--row (key &rest fields)
  "Return the session KEY as the Gateway lists it, with FIELDS, a plist,
and those of a conversation of yours otherwise."
  (append (list :key key) fields
          (list :kind "direct" :agentId "main" :spawnDepth 0)))

(defun lispm-openclaw-test--wait (predicate)
  "Wait until PREDICATE holds, for five seconds at most."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))))

;;;; The agent

(ert-deftest lispm-openclaw-test-defined-on-load ()
  "Loaded, OpenClaw is one of the editor's agents, taking part while its
command is on PATH, started with k; its sessions are read for lists of
them without waiting."
  (let ((props (alist-get 'openclaw lispm-agents-defined)))
    (should (equal (plist-get props :name) "OpenClaw"))
    (should (equal (plist-get props :program) "openclaw"))
    (should (equal (plist-get props :key) "k"))
    (should (= (string-width (plist-get props :text-icon)) 1))
    (should-not (plist-get props :built-in))
    (should (eq (plist-get props :context) #'lispm-openclaw--context))
    (should (eq (plist-get props :deliver) #'lispm-openclaw--deliver))
    (should (eq (plist-get props :answer) #'lispm-openclaw--answer-request))
    (should (file-readable-p (nth 1 (plist-get props :icon)))))
  (should (eq (alist-get 'openclaw lispm-agents--session-readers) #'lispm-openclaw--read-sessions))
  (should (memq #'lispm-openclaw--reread lispm-agents-reread-sessions-hook))
  (cl-letf (((symbol-function 'executable-find) #'ignore))
    (should-not (lispm-agents-available-p 'openclaw)))
  (cl-letf (((symbol-function 'executable-find) (lambda (program &rest _) (concat "/bin/" program))))
    (should (lispm-agents-available-p 'openclaw))
    (should (eq (keymap-lookup lispm-agents-map "k") 'lispm-openclaw-new))))

(ert-deftest lispm-openclaw-test-unload ()
  "Unloading takes OpenClaw away from the agents, with its reader of
sessions; loading it again brings it back."
  (unwind-protect
      (progn
        (lispm-openclaw-unload-function)
        (should-not (assq 'openclaw lispm-agents-defined))
        (should-not (assq 'openclaw lispm-agents--session-readers))
        (should-not (memq #'lispm-openclaw--reread lispm-agents-reread-sessions-hook)))
    (load "lispm-openclaw" nil t))
  (should (assq 'openclaw lispm-agents-defined))
  (should (assq 'openclaw lispm-agents--session-readers)))

(ert-deftest lispm-openclaw-test-state-directory ()
  "OpenClaw's state is in OPENCLAW_STATE_DIR, else ~/.openclaw."
  (let ((process-environment (cons "OPENCLAW_STATE_DIR=/srv/claw" process-environment)))
    (should (equal (lispm-openclaw-state-directory) "/srv/claw/")))
  (dolist (unset '("OPENCLAW_STATE_DIR" "OPENCLAW_STATE_DIR="))
    (let ((process-environment (cons unset process-environment)))
      (should (equal (lispm-openclaw-state-directory) (expand-file-name "~/.openclaw/"))))))

;;;; The Gateway

(ert-deftest lispm-openclaw-test-arguments ()
  "A call of the Gateway asks for JSON, waits as long as the Gateway has to
answer, and passes its parameters as JSON, text as text."
  (should (equal (lispm-openclaw--arguments "sessions.patch" '(:key "agent:main:a" :label "Café"))
                 (list "gateway" "call" "sessions.patch" "--json" "--timeout" "10000"
                       "--params" "{\"key\":\"agent:main:a\",\"label\":\"Café\"}")))
  (should (multibyte-string-p (lispm-openclaw--encode '(:label "Café"))))
  (should (equal (car (last (lispm-openclaw--arguments "health" nil))) "{}")))

(ert-deftest lispm-openclaw-test-answer ()
  "The answer is the JSON printed, after whatever came before it; the
Gateway's errors say what it said, its being unreachable apart from its
refusing, as both newer OpenClaws, in JSON, and older ones, in words,
say them."
  (should (equal (lispm-openclaw--answer "{\"ok\":true,\"key\":\"agent:main:a\",\"x\":[1,null]}" "")
                 '(:ok t :key "agent:main:a" :x (1 nil))))
  (should (equal (lispm-openclaw--answer "Config warnings: none\n{\"sessions\":[]}\n" "")
                 '(:sessions nil)))
  (should (equal (should-error (lispm-openclaw--answer
                                "{\"ok\":false,\"error\":{\"type\":\"gateway_transport_error\",\"message\":\"Gateway not reachable at ws://127.0.0.1:18789 (ECONNREFUSED).\"}}"
                                "")
                               :type 'lispm-openclaw-unreachable)
                 '(lispm-openclaw-unreachable "Gateway not reachable at ws://127.0.0.1:18789 (ECONNREFUSED).")))
  (should (equal (should-error (lispm-openclaw--answer
                                "{\"ok\":false,\"error\":{\"code\":\"INVALID_REQUEST\",\"message\":\"label already in use: A\\nmore\"}}"
                                "")
                               :type 'lispm-openclaw-refused)
                 '(lispm-openclaw-refused "label already in use: A")))
  (should (equal (should-error (lispm-openclaw--answer
                                "" "Gateway call failed: Error: gateway timeout after 10000ms\nGateway target: wss://h:8443\n")
                               :type 'lispm-openclaw-unreachable)
                 '(lispm-openclaw-unreachable "gateway timeout after 10000ms")))
  (should (equal (should-error (lispm-openclaw--answer
                                "" "[openclaw] Failed to start CLI: Error: unknown method: sessions.nope\n")
                               :type 'lispm-openclaw-refused)
                 '(lispm-openclaw-refused "unknown method: sessions.nope")))
  (should (equal (should-error (lispm-openclaw--answer
                                "" "Gateway call failed: GatewayClientRequestError: invalid sessions.create params: at root: unexpected property 'cwd'\n")
                               :type 'lispm-openclaw-refused)
                 '(lispm-openclaw-refused "invalid sessions.create params: at root: unexpected property 'cwd'")))
  (should (equal (cdr (should-error (lispm-openclaw--answer "" "") :type 'lispm-openclaw-refused))
                 '("it said nothing")))
  ;; Its errors are a person's to read, not a program's to debug.
  (should (equal (lispm-openclaw--error-message '(lispm-openclaw-refused "label already in use: A"))
                 "OpenClaw: label already in use: A"))
  (should (equal (lispm-openclaw--error-message '(lispm-openclaw-unreachable "gateway closed"))
                 "OpenClaw's Gateway cannot be reached: gateway closed"))
  (should (get 'lispm-openclaw-unreachable 'error-conditions))
  (should (memq 'user-error (get 'lispm-openclaw-unreachable 'error-conditions))))

(ert-deftest lispm-openclaw-test-call ()
  "A call runs `openclaw gateway call' and returns the Gateway's answer, or
signals its error, read from its output or its error output."
  (lispm-openclaw-test-with-gateway
      '(("health" . (:ok t :status "live"))
        ("sessions.delete" . (:json (:ok :false :error (:message "Cannot delete the main session"))
                              :status 1))
        ("sessions.list" . (:err "Gateway call failed: Error: gateway closed (1006): \n" :status 1)))
    (should (equal (lispm-openclaw--call "health") '(:ok t :status "live")))
    (should (equal (calls) '("gateway call health --json --timeout 10000 --params {}")))
    (should (equal (cdr (should-error (lispm-openclaw--call "sessions.delete" '(:key "agent:main:main"))
                                      :type 'lispm-openclaw-refused))
                   '("Cannot delete the main session")))
    (should (equal (params "sessions.delete") '((:key "agent:main:main"))))
    (should-error (lispm-openclaw--call "sessions.list") :type 'lispm-openclaw-unreachable)))

(ert-deftest lispm-openclaw-test-call-in-the-background ()
  "A call in the background hands its callback the answer, or the error."
  (lispm-openclaw-test-with-gateway
      '(("health" . (:ok t))
        ("sessions.list" . (:err "Gateway call failed: Error: gateway timeout after 10000ms" :status 1)))
    (let ((results nil))
      (lispm-openclaw--call-async "health" nil (lambda (answer err) (push (list answer err) results)))
      (lispm-openclaw--call-async "sessions.list" nil (lambda (answer err) (push (list answer err) results)))
      (lispm-openclaw-test--wait (lambda () (= (length results) 2)))
      (should (member '((:ok t) nil) results))
      (should (member '(nil (lispm-openclaw-unreachable "gateway timeout after 10000ms")) results)))))

;;;; Sessions

(ert-deftest lispm-openclaw-test-session-of-a-row ()
  "A session is titled by its label, else by the name OpenClaw gave it,
else by its last message, else by its key; it belongs to the directory
it was started in when that is here, else it is a chat; it is as recent
as it was last talked to."
  (let ((here (make-temp-file "lispm-openclaw-test-project" t)))
    (unwind-protect
        (progn
          (should (equal (lispm-openclaw--session
                          (lispm-openclaw-test--row "agent:main:lispm:a1" :label "Fix it"
                                                    :displayName "Ignored" :spawnedCwd (concat here "/")
                                                    :lastInteractionAt 1792755858172
                                                    :updatedAt 1792755880364))
                         (list :agent 'openclaw :id "agent:main:lispm:a1" :cwd here :title "Fix it"
                               :time 1792755858.172)))
          (let ((session (lispm-openclaw--session
                          (lispm-openclaw-test--row "agent:main:main" :spawnedCwd "/nowhere/here"
                                                    :lastMessagePreview "Echo:  a\n\nreply"
                                                    :lastInteractionAt 1000))))
            (should (equal (plist-get session :cwd) (expand-file-name "~")))
            (should (equal (plist-get session :title) "Echo: a"))
            (should (equal (plist-get session :time) 1.0)))
          (should (equal (plist-get (lispm-openclaw--session
                                     (lispm-openclaw-test--row "agent:main:telegram:direct:42"
                                                               :displayName "telegram:Ada"
                                                               :lastInteractionAt 1))
                                    :title)
                         "telegram:Ada"))
          (should (equal (plist-get (lispm-openclaw--session
                                     (lispm-openclaw-test--row "agent:work:lispm:b2" :lastInteractionAt 1))
                                    :title)
                         "lispm:b2"))
          ;; Older Gateways name sessions after the program talking in
          ;; them, and derive titles from metadata.
          (should (equal (plist-get (lispm-openclaw--session
                                     (lispm-openclaw-test--row
                                      "agent:main:lispm:c3" :displayName "openclaw-tui"
                                      :derivedTitle "Sender (untrusted metadata): ```json { \"label\":…"
                                      :lastMessagePreview "Changelog written." :lastInteractionAt 1))
                                    :title)
                         "Changelog written."))
          (should-not (lispm-openclaw--session
                       (lispm-openclaw-test--row "agent:main:lispm:d4" :displayName "openclaw-tui"))))
      (delete-directory here t))))

(ert-deftest lispm-openclaw-test-conversations-listed ()
  "Listed are your own conversations with OpenClaw's agents that were named
or said something; not those of groups, subagents, scheduled jobs or the
background, nor archived ones, nor ones never talked to."
  (let ((rows (list (lispm-openclaw-test--row "agent:main:main" :lastInteractionAt 1)
                    (lispm-openclaw-test--row "agent:main:named" :label "Named")
                    (lispm-openclaw-test--row "agent:main:discord:group:9" :kind "group"
                                              :lastInteractionAt 1)
                    (lispm-openclaw-test--row "agent:main:subagent:c3" :lastInteractionAt 1)
                    (lispm-openclaw-test--row "agent:main:child" :spawnDepth 1 :lastInteractionAt 1)
                    (lispm-openclaw-test--row "agent:main:spawned" :spawnedBy "agent:main:main"
                                              :lastInteractionAt 1)
                    (lispm-openclaw-test--row "agent:main:cron:job" :lastInteractionAt 1)
                    (lispm-openclaw-test--row "cron:job" :lastInteractionAt 1)
                    (lispm-openclaw-test--row "agent:main:heartbeat" :isBackground t :lastInteractionAt 1)
                    (lispm-openclaw-test--row "agent:main:old" :archived t :lastInteractionAt 1)
                    (lispm-openclaw-test--row "agent:main:dashboard:d4"))))
    (should (equal (mapcar (lambda (session) (plist-get session :id))
                           (lispm-openclaw--sessions-of (list :sessions rows)))
                   '("agent:main:main" "agent:main:named")))))

(ert-deftest lispm-openclaw-test-short-id ()
  "A session's short id is the last part of its key, eight characters at
most."
  (should (equal (lispm-openclaw--short-id "agent:main:lispm:758035e9-b0ef-4eb3-84d2-6966cb07064b")
                 "758035e9"))
  (should (equal (lispm-openclaw--short-id "agent:main:main") "main"))
  (should (equal (lispm-openclaw--key-rest "agent:main:telegram:direct:42") "telegram:direct:42"))
  (should (equal (lispm-openclaw--key-rest "global") "global")))

(ert-deftest lispm-openclaw-test-fresh ()
  "The sessions known hold while OpenClaw's store here is unchanged and
the interval has not passed."
  (let ((lispm-openclaw-refresh-interval 30)
        (cache '(:stamp (("a" . 1.0)) :time 100.0 :sessions nil)))
    (should (lispm-openclaw--fresh-p cache '(("a" . 1.0)) 110.0))
    (should-not (lispm-openclaw--fresh-p cache '(("a" . 2.0)) 110.0))
    (should-not (lispm-openclaw--fresh-p cache '(("a" . 1.0)) 131.0))
    (should-not (lispm-openclaw--fresh-p nil nil 110.0))))

(ert-deftest lispm-openclaw-test-sessions-from-the-gateway ()
  "The sessions are asked of the Gateway, the most recent with what titles
them, and again only once OpenClaw's store here changed, or the
interval passed."
  (lispm-openclaw-test-with-gateway
      `(("sessions.list" . (:sessions [,(lispm-openclaw-test--row "agent:main:a" :label "A"
                                                                  :lastInteractionAt 2000)])))
    (let ((database (expand-file-name "agents/main/agent/openclaw-agent.sqlite" state)))
      (make-directory (file-name-directory database) t)
      (write-region "" nil database nil 'silent)
      (should (equal (mapcar (lambda (session) (plist-get session :title)) (lispm-openclaw-sessions))
                     '("A")))
      (should (equal (params "sessions.list")
                     '((:limit 100 :includeDerivedTitles t :includeLastMessage t))))
      (lispm-openclaw-sessions)
      (should (= (length (calls)) 1))
      (set-file-times database (time-add nil 10))
      (lispm-openclaw-sessions)
      (should (= (length (calls)) 2))
      (let ((lispm-openclaw-refresh-interval 0))
        (lispm-openclaw-sessions))
      (should (= (length (calls)) 3)))))

(ert-deftest lispm-openclaw-test-sessions-unavailable ()
  "A Gateway that cannot be asked lists no sessions, and says so, once."
  (lispm-openclaw-test-with-gateway
      '(("sessions.list" . (:json (:ok :false :error (:type "gateway_transport_error"
                                                       :message "Gateway not reachable at ws://127.0.0.1:18789 (ECONNREFUSED)."))
                            :status 1)))
    (let ((said nil)
          (lispm-openclaw-refresh-interval 0))
      (cl-letf (((symbol-function 'message) (lambda (format &rest args)
                                              (push (apply #'format-message format args) said))))
        (should-not (lispm-openclaw-sessions))
        (should-not (lispm-openclaw-sessions)))
      (should (= (length (calls)) 2))
      (should (equal said (list (concat "OpenClaw sessions unavailable: OpenClaw's Gateway cannot be "
                                        "reached: Gateway not reachable at ws://127.0.0.1:18789 "
                                        "(ECONNREFUSED).")))))))

(ert-deftest lispm-openclaw-test-reader ()
  "Lists of the sessions have them without waiting: pending until the
Gateway first answers in the background, which tells the lists; then
those known, asked again when they may be stale, not while they are
being asked, nor again a moment after."
  (lispm-openclaw-test-with-gateway
      `(("sessions.list" . (:sessions [,(lispm-openclaw-test--row "agent:main:a" :label "A"
                                                                  :lastInteractionAt 2000)]))
        ("sessions.list.2" . (:sessions [,(lispm-openclaw-test--row "agent:main:a" :label "A"
                                                                    :lastInteractionAt 2000)
                                         ,(lispm-openclaw-test--row "agent:main:b" :label "B"
                                                                    :lastInteractionAt 3000)])))
    (let ((arrived 0)
          (lispm-agents-sessions-arrived-hook nil))
      (add-hook 'lispm-agents-sessions-arrived-hook (lambda () (cl-incf arrived)))
      (should (eq (lispm-openclaw--read-sessions) 'pending))
      (should (eq (lispm-openclaw--read-sessions) 'pending))
      (lispm-openclaw-test--wait (lambda () (= arrived 1)))
      (should (= (length (calls)) 1))
      (should (equal (mapcar (lambda (session) (plist-get session :id)) (lispm-openclaw--read-sessions))
                     '("agent:main:a")))
      ;; Asked to read anew, the reader shows what it has meanwhile.
      (lispm-openclaw--reread)
      (should (equal (mapcar (lambda (session) (plist-get session :id)) (lispm-openclaw--read-sessions))
                     '("agent:main:a")))
      (lispm-openclaw-test--wait (lambda () (= arrived 2)))
      (should (equal (mapcar (lambda (session) (plist-get session :id)) (lispm-openclaw--read-sessions))
                     '("agent:main:a" "agent:main:b")))
      (should (= (length (calls)) 2))
      ;; Stale again a moment after, it waits for the pause to pass.
      (setq lispm-openclaw--cache (plist-put lispm-openclaw--cache :time 0))
      (lispm-openclaw--read-sessions)
      (should (= (length (calls)) 2)))))

(ert-deftest lispm-openclaw-test-rename-and-delete ()
  "Renaming gives a session its label, deleting deletes it with its
transcript; both show at once, and the sessions are asked again.  What
the Gateway refuses is said."
  (lispm-openclaw-test-with-gateway
      '(("sessions.patch" . (:ok t))
        ("sessions.delete" . (:ok t :deleted t)))
    (setq lispm-openclaw--cache
          (list :stamp nil :time (float-time)
                :sessions (list '(:agent openclaw :id "agent:main:a" :title "A")
                                '(:agent openclaw :id "agent:main:b" :title "B"))))
    (lispm-openclaw--rename '(:id "agent:main:a") "Café")
    (should (equal (params "sessions.patch") '((:key "agent:main:a" :label "Café"))))
    (should (equal (mapcar (lambda (session) (plist-get session :title))
                           (plist-get lispm-openclaw--cache :sessions))
                   '("Café" "B")))
    (lispm-openclaw--delete '(:id "agent:main:b"))
    (should (equal (params "sessions.delete") '((:key "agent:main:b" :deleteTranscript t))))
    (should (equal (mapcar (lambda (session) (plist-get session :id))
                           (plist-get lispm-openclaw--cache :sessions))
                   '("agent:main:a")))
    (should (= (plist-get lispm-openclaw--cache :time) 0)))
  (lispm-openclaw-test-with-gateway
      '(("sessions.patch" . (:json (:ok :false :error (:message "label already in use: B")) :status 1)))
    (should (equal (error-message-string
                    (should-error (lispm-openclaw--rename '(:id "agent:main:a") "B") :type 'user-error))
                   "OpenClaw: label already in use: B"))))

;;;; Transcripts

(ert-deftest lispm-openclaw-test-markdown ()
  "A transcript is what you wrote and what OpenClaw answered, without
tools, thoughts or what is empty."
  (should (equal (lispm-openclaw--markdown
                  "Fix it"
                  (list (list :role "user" :content "Fix the parser" :timestamp 0)
                        (list :role "assistant"
                              :content (list '(:type "thinking" :thinking "Hmm")
                                             '(:type "toolCall" :name "read")))
                        (list :role "toolResult" :content "file")
                        (list :role "assistant"
                              :content (list '(:type "text" :text "Done.") '(:type "text" :text " Twice. ")))))
                 (format "# Fix it\n\n## You%s\n\nFix the parser\n\n## OpenClaw\n\nDone.\n\n Twice.\n"
                         (format-time-string " · %F %R" 0)))))

(ert-deftest lispm-openclaw-test-transcript ()
  "A session's transcript is its conversation as the Gateway gives it, the
latest messages, in a Markdown file."
  (lispm-openclaw-test-with-gateway
      '(("chat.history" . (:sessionKey "agent:main:a" :messages [(:role "user" :content "Hi" :timestamp 0)
                                                                   (:role "assistant" :content "Hello")])))
    (let ((file (lispm-openclaw--transcript '(:id "agent:main:a" :title "Greeting"))))
      (unwind-protect
          (progn
            (should (string-suffix-p ".md" file))
            (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                           (lispm-openclaw--markdown "Greeting" '((:role "user" :content "Hi" :timestamp 0)
                                                                 (:role "assistant" :content "Hello")))))
            (should (equal (params "chat.history") '((:sessionKey "agent:main:a" :limit 1000)))))
        (delete-file file)))))

;;;; Starting and resuming

(ert-deftest lispm-openclaw-test-new-key ()
  "A new session's key is LispM's, of the default agent, else of
`lispm-openclaw-agent'."
  (should (string-match-p "\\`lispm:[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-4[0-9a-f]\\{3\\}-[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}\\'"
                          (lispm-openclaw--new-key)))
  (should-not (equal (lispm-openclaw--new-key) (lispm-openclaw--new-key)))
  (let ((lispm-openclaw-agent "work"))
    (should (string-prefix-p "agent:work:lispm:" (lispm-openclaw--new-key)))))

(ert-deftest lispm-openclaw-test-new-session-in-a-project ()
  "A new session is made in the project and opened in the terminal UI by
the key the Gateway gave it; outside projects it is a chat."
  (lispm-openclaw-test-with-gateway
      '(("sessions.create" . (:ok t :key "agent:main:lispm:a1")))
    (cl-letf (((symbol-function 'lispm-openclaw--project) (lambda () "/src/demo")))
      (should (equal (lispm-openclaw--new-arguments)
                     '("agent:main:lispm:a1" "tui" "--session" "agent:main:lispm:a1"))))
    (let ((sent (car (params "sessions.create"))))
      (should (string-prefix-p "lispm:" (plist-get sent :key)))
      (should (equal (plist-get sent :cwd) "/src/demo")))
    (cl-letf (((symbol-function 'lispm-openclaw--project) #'ignore))
      (lispm-openclaw--new-arguments))
    (should-not (plist-member (nth 1 (params "sessions.create")) :cwd)))
  (should (equal (lispm-openclaw--resume-arguments "agent:main:main") '("tui" "--session" "agent:main:main"))))

(ert-deftest lispm-openclaw-test-new-session-elsewhere ()
  "A Gateway refusing to work in the project, as an older one does, makes
a chat, which is said; one that cannot be reached is said to be, with
how to start it."
  (lispm-openclaw-test-with-gateway
      '(("sessions.create" . (:json (:ok :false :error (:message "invalid sessions.create params: at root: unexpected property 'cwd'"))
                              :status 1))
        ("sessions.create.2" . (:ok t)))
    (let ((said nil))
      (cl-letf (((symbol-function 'message) (lambda (format &rest args)
                                              (push (apply #'format-message format args) said))))
        (let ((key (lispm-openclaw--create "/src/demo")))
          (should (string-prefix-p "lispm:" key))
          (should (equal (mapcar (lambda (params) (plist-get params :key)) (params "sessions.create"))
                         (list key key)))))
      (should-not (plist-member (nth 1 (params "sessions.create")) :cwd))
      (should (equal said '("OpenClaw cannot work in /src/demo, so this is a chat: invalid sessions.create params: at root: unexpected property 'cwd'")))))
  (lispm-openclaw-test-with-gateway
      '(("sessions.create" . (:err "Gateway call failed: Error: gateway timeout after 10000ms" :status 1)))
    (should (string-match-p "cannot be reached (gateway timeout after 10000ms): .openclaw gateway start. starts it"
                            (cadr (should-error (lispm-openclaw--create nil) :type 'user-error))))))

;;;; Other agents' messages, and LispM's tools

(defmacro lispm-openclaw-test-with-terminal (key &rest body)
  "Run BODY with `terminal' a live terminal of the session KEY."
  (declare (indent 1))
  `(let ((terminal (generate-new-buffer "*openclaw:test*"))
         (lispm-agents-terminal-live-p-function #'buffer-live-p))
     (unwind-protect
         (progn (with-current-buffer terminal (setq lispm-agents-buffer-session-id ,key))
                ,@body)
       (kill-buffer terminal))))

(ert-deftest lispm-openclaw-test-reply-of ()
  "A session's final reply to a message is the first answer after it that
ends a run; till there is one, there is none.  The message is known by
its idempotency key, or, where the Gateway keeps none, by its text."
  (let* ((sent (list :role "user" :content "[Request R1 from codex]\n2+2?" :idempotencyKey "k1:user"))
         (thinking (list :role "assistant" :stopReason "toolUse"
                         :content (list (list :type "text" :text "Let me see.")
                                        (list :type "toolCall" :name "exec"))))
         (final (list :role "assistant" :stopReason "stop" :content (list (list :type "text" :text "4"))))
         (later (list :role "assistant" :stopReason "stop" :content "Anything else?")))
    (should-not (lispm-openclaw--reply-of (list :messages (list sent thinking)) "k1" "x"))
    (should (equal (lispm-openclaw--reply-of (list :messages (list final sent thinking final later)) "k1" "x")
                   "4"))
    (should-not (lispm-openclaw--reply-of (list :messages (list sent final)) "k2" "x"))
    (should (equal (lispm-openclaw--reply-of (list :messages (list (list :role "user" :content " 2+2? ") final))
                                             "k2" "2+2?")
                   "4"))))

(ert-deftest lispm-openclaw-test-deliver ()
  "Another agent's message goes to the Gateway as the session's user's next
message, kept for after its run under way; an older Gateway, knowing no
queueMode, is given it without."
  (lispm-openclaw-test-with-gateway
      '(("chat.send" . (:runId "k" :status "started")))
    (lispm-openclaw-test-with-terminal "agent:main:lispm:a1"
      (should (eq (lispm-openclaw--deliver terminal "[Message from codex]\nHi.") t))
      (lispm-openclaw-test--wait (lambda () (params "chat.send")))
      (let ((sent (car (params "chat.send"))))
        (should (equal (plist-get sent :sessionKey) "agent:main:lispm:a1"))
        (should (equal (plist-get sent :message) "[Message from codex]\nHi."))
        (should (equal (plist-get sent :queueMode) "followup"))
        (should (stringp (plist-get sent :idempotencyKey))))))
  (lispm-openclaw-test-with-gateway
      '(("chat.send" . (:json (:ok :false :error (:message "invalid chat.send params: at root: unexpected property 'queueMode'"))
                        :status 1))
        ("chat.send.2" . (:runId "k" :status "started")))
    (lispm-openclaw-test-with-terminal "agent:main:lispm:a1"
      (lispm-openclaw--deliver terminal "Hi.")
      (lispm-openclaw-test--wait (lambda () (= (length (params "chat.send")) 2)))
      (should (plist-member (nth 0 (params "chat.send")) :queueMode))
      (should-not (plist-member (nth 1 (params "chat.send")) :queueMode))
      (should (equal (plist-get (nth 0 (params "chat.send")) :idempotencyKey)
                     (plist-get (nth 1 (params "chat.send")) :idempotencyKey))))))

(ert-deftest lispm-openclaw-test-answer-request ()
  "A session served no tools answers a request with its final reply to it,
looked for in its history till it is there."
  (lispm-openclaw-test-with-gateway
      '(("chat.send" . (:runId "k" :status "started")))
    (lispm-openclaw-test-with-terminal "agent:main:lispm:a1"
      (let ((lispm-openclaw-answer-interval 0.05)
            (answered nil))
        (lispm-openclaw--answer-request terminal "2+2?" (lambda (reply) (setq answered reply)))
        (lispm-openclaw-test--wait (lambda () (params "chat.send")))
        (let ((id (plist-get (car (params "chat.send")) :idempotencyKey)))
          (lispm-openclaw-test--answer
           fake-directory "chat.history"
           (list :messages (vector (list :role "user" :content "2+2?" :idempotencyKey (concat id ":user")))))
          (lispm-openclaw-test--wait (lambda () (params "chat.history")))
          (should-not answered)
          (lispm-openclaw-test--answer
           fake-directory "chat.history"
           (list :messages (vector (list :role "user" :content "2+2?" :idempotencyKey (concat id ":user"))
                                   (list :role "assistant" :stopReason "stop" :content "4")))))
        (lispm-openclaw-test--wait (lambda () answered))
        (should (equal answered "4"))
        (should (equal (plist-get (car (params "chat.history")) :sessionKey) "agent:main:lispm:a1"))))))

(ert-deftest lispm-openclaw-test-context ()
  "While LispM runs, a session in a terminal is a peer: with LispM's tools
through a server of its own in a Gateway here, on for it alone and gone
with it; else, as with a Gateway elsewhere or one that cannot, with none."
  (skip-unless (fboundp 'lispm-agents-context-presence-make))
  (lispm-openclaw-test-with-gateway
      '(("sessions.patch" . (:ok t)))
    (let ((lispm-openclaw--served nil)
          (lispm-openclaw--swept nil))
      (cl-letf (((symbol-function 'lispm-openclaw--messaging-p) #'ignore))
        (should-not (lispm-openclaw--context state "agent:main:lispm:a1")))
      (cl-letf (((symbol-function 'lispm-openclaw--messaging-p) #'always))
        (should-not (lispm-openclaw--context state))
        ;; A Gateway elsewhere.
        (with-temp-file (expand-file-name "config-get.out" fake-directory) (insert "remote\n"))
        (should (lispm-agents-context-presence-p (lispm-openclaw--context state "agent:main:lispm:a1")))
        (should-not (params "sessions.patch"))
        ;; A Gateway here.
        (with-temp-file (expand-file-name "config-get.out" fake-directory) (insert "local\n"))
        (with-temp-file (expand-file-name "mcp-list.out" fake-directory)
          (insert "{\"lispm_0123456789\": {\"enabled\": false}, \"docs\": {\"command\": \"x\"}}\n"))
        (let* ((server (lispm-openclaw--context state "agent:main:lispm:a1"))
               (name (lispm-openclaw--server-name server))
               (set (seq-find (lambda (call) (string-prefix-p "mcp set " call)) (calls))))
          (should (lispm-openclaw-mcp-p server))
          ;; One left by an Emacs no longer running goes; others stay.
          (should (member "mcp unset lispm_0123456789" (calls)))
          (should-not (seq-find (lambda (call) (string-search "unset docs" call)) (calls)))
          (should (string-prefix-p (format "mcp set %s {" name) set))
          (let ((config (lispm-openclaw--json (substring set (+ 9 (length name))))))
            (should (equal (plist-get config :url)
                           (format "http://127.0.0.1:%d/mcp" (lispm-agents-mcp-port server))))
            (should (equal (plist-get config :transport) "streamable-http"))
            (should (equal (plist-get (plist-get config :headers) :Authorization)
                           (concat "Bearer " (lispm-agents-mcp-token server))))
            (should (plist-member config :enabled))
            (should-not (plist-get config :enabled)))
          (should (equal (car (params "sessions.patch"))
                         (list :key "agent:main:lispm:a1"
                               :toolOverrides (list :mcpServers (list (intern (concat ":" name)) t)))))
          (lispm-agents-context-stop server)
          (should-not lispm-openclaw--served)
          (lispm-openclaw-test--wait (lambda () (member (concat "mcp unset " name) (calls))))
          (should (member (concat "mcp unset " name) (calls))))
        ;; A Gateway that cannot turn a server on for one session.
        (lispm-openclaw-test--answer
         fake-directory "sessions.patch"
         '(:json (:ok :false :error (:message "invalid sessions.patch params: at root: unexpected property 'toolOverrides'"))
                 :status 1))
        (cl-letf (((symbol-function 'message) #'ignore))
          (should (lispm-agents-context-presence-p (lispm-openclaw--context state "agent:main:lispm:a2"))))
        (let ((names (delq nil (mapcar (lambda (call) (and (string-match "\\`mcp set \\([^ ]+\\)" call)
                                                            (match-string 1 call)))
                                       (calls)))))
          (should (member (concat "mcp unset " (car (last names))) (calls))))
        (should-not lispm-openclaw--served)))))

(ert-deftest lispm-openclaw-test-knows-no-desktop ()
  "OpenClaw's extension stands alone beside lispm-agent: none of its files
but the tests names LispM Desktop's code."
  (dolist (file (directory-files (file-name-directory (locate-library "lispm-openclaw")) t "\\.el\\'"))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not (and (re-search-forward "lispm-desktop\\|platinum" nil t)
                       (format "%s:%d" (file-name-nondirectory file) (line-number-at-pos)))))))

(provide 'lispm-openclaw-test)
;;; lispm-openclaw-test.el ends here
