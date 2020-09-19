(define-module (i3 core)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  #:export (;; Connection
            i3-ipc-connection?
            connect-to-i3
            ;; Reply
            i3-reply?
            i3-reply-type
            i3-reply-body
            ;; Event
            i3-event?
            i3-event-type
            i3-event-body
            ;; Functions
            i3-send-message!
            i3-receive-reply))

(define i3-magic-string (string->utf8 "i3-ipc"))

(define-record-type <i3-ipc-connection>
  (make-i3-ipc-internal sock sender receiver)
  i3-ipc-connection?
  ;; Socket
  (sock i3-ipc-sock)
  ;; Internals
  (sender i3-ipc-sender)
  (receiver i3-ipc-receiver))

(define-record-type <i3-reply>
  (make-i3-reply type body)
  i3-reply?
  (type i3-reply-type)
  (body i3-reply-body))

(define-record-type <i3-event>
  (make-i3-event type body)
  i3-event?
  (type i3-event-type)
  (body i3-event-body))

(define (make-i3-ipc sock)
  (make-i3-ipc-internal
   sock
   (make-i3-ipc-sender sock)
   (make-i3-ipc-receiver sock)))

;;;; Connecting

;; TODO: Change for sway
(define (get-socket-path)
  (getenv "I3SOCK"))

(define (make-i3-sock sock-path)
  (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
    (connect sock AF_UNIX sock-path)
    sock))

;;;; Internals

;;; Sender

(define (make-i3-ipc-sender sock)
  (let ((header-buffer (make-bytevector (+ 4 4))))
    (letrec ((send-buffer (lambda (buffer)
                            (let ((sent (send sock buffer)))
                              (when (< sent (bytevector-length buffer))
                                (error "Not everything sent!"))))))
      (lambda (type body)
        (let* ((body-buffer (and (< 0 (string-length body))
                                 (string->utf8 body)))
               (body-length (if body-buffer (bytevector-length body-buffer) 0)))
          ;; Fill header
          (bytevector-u32-set! header-buffer 0 body-length (native-endianness))
          (bytevector-u32-set! header-buffer 4 type (native-endianness))
          ;; Send
          (send-buffer i3-magic-string)
          (send-buffer header-buffer)
          (when body-buffer
            (send-buffer body-buffer))
          #nil)))))

;;; Receiver

(define in-buffer-size 1024)

(define (make-i3-ipc-receiver sock)
  (let ((in-buffer (make-bytevector in-buffer-size))
        (in-buffer-pos 0)
        (in-buffer-left 0)

        (in-magic-string (make-bytevector (bytevector-length i3-magic-string)))
        (in-msg-header (make-bytevector (+ 4 4))))
    (letrec ((maybe-recv!
              (lambda ()
                (unless (< in-buffer-pos in-buffer-left)
                  (set! in-buffer-pos 0)
                  (set! in-buffer-left (recv! sock in-buffer)))))
             (next-byte
              (lambda ()
                (maybe-recv!)
                (let ((byte (array-ref in-buffer in-buffer-pos)))
                  (set! in-buffer-pos (1+ in-buffer-pos))
                  byte)))
             (read-into!
              (lambda (buffer)
                (do ((i 0 (1+ i)))
                    ((>= i (bytevector-length buffer)))
                  (array-set! buffer (next-byte) i)))))
      (lambda ()
        ;; Read magic string
        (read-into! in-magic-string)
        (unless (bytevector=? i3-magic-string in-magic-string)
          (error "Wrong magic string"))
        ;; Read header
        (read-into! in-msg-header)
        (let* ((body-length (bytevector-u32-ref in-msg-header 0 (native-endianness)))
               (reply-type (bytevector-u32-ref in-msg-header 4 (native-endianness)))
               (body-buffer (make-bytevector body-length)))
          ;; Read body
          (read-into! body-buffer)
          ;; Create reply object
          (if (logbit? 31 reply-type)
              (make-i3-event (logxor reply-type #x80000000) (utf8->string body-buffer))
              (make-i3-reply reply-type (utf8->string body-buffer))))))))

;;;; Low level

(define (connect-to-i3)
  (make-i3-ipc (make-i3-sock (get-socket-path))))

(define (i3-disconnect! i3-ipc)
  (shutdown (i3-ipc-sock i3-ipc) 2))

(define (i3-send-message! i3-ipc type body)
  ((i3-ipc-sender i3-ipc) type body)
  i3-ipc)

(define (i3-receive-reply i3-ipc)
  ((i3-ipc-receiver i3-ipc)))
