;; BSD 3-Clause License
;;
;; Copyright (c) 2018, Matti J. Kärki
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; * Redistributions of source code must retain the above copyright notice, this
;;   list of conditions and the following disclaimer.
;;
;; * Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation
;;   and/or other materials provided with the distribution.
;;
;; * Neither the name of the copyright holder nor the names of its
;;   contributors may be used to endorse or promote products derived from
;;   this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(load "nativewin.scm")
(load "misctools.scm")
(load "match.scm")

(define HOST "gfwsl.geforce.com")
(define PATH "/services_toolkit/services/com/nvidia/services/AjaxDriverService.php?func=DriverManualLookup&psid=101&pfid=859&osID=57&languageCode=1033&beta=0&isWHQL=1&dltype=-1&sort1=0&numberOfResults=10")
(define PROGRAMFILES (getenv "ProgramFiles"))
(define NVIDIASMIPATH "NVIDIA Corporation\\NVSMI\\nvidia-smi.exe")
(define NVIDIASMI (string-append PROGRAMFILES "\\" NVIDIASMIPATH))
(define WINDIR (getenv "windir"))
(define CMD (string-append WINDIR "\\System32\\" "cmd.exe"))

(define (show-error-message nvidiasmi)
  (message-box (string-append "Unable to get the driver version!"
                              "\n\n"
                              "Can't get information by using the program:"
                              "\n"
                              nvidiasmi
                              "\n\n"
                              "Maybe the GeForce drivers are not installed?")
               "Error!"
               (bitwise-ior MB_OK MB_ICONSTOP))
  #f)

(define (get-installed-version)
  (let ((matchresult (match "Driver Version: (N.N)"
                            (with-exception-handler
                             (lambda (e)
                               "")
                             (lambda ()
                               (call-with-input-process (list path: NVIDIASMI
                                                              show-console: #f)
                                                        port->string))))))
    (cond
     ((> (string-length matchresult) 0)
      (string->number matchresult))
     (else
      (show-error-message NVIDIASMI)))))

(define (get-driver-info)
  (let ((page (winhttp-get HOST PATH #t)))
    (cond
     ((string? page)
      (list (string->number (match "\"Version\" : \"(N.N)\"" page))
            (match "\"DownloadURL\" : \"(*)\"" page)))
     (else
      (list #f #f)))))

(define (ask-for-update installed-version version)
  (message-box (string-append "Current version: "
                              installed-version
                              "\n"
                              "New version: "
                              version)
               "There is a new GeForce driver available"
               (bitwise-ior MB_YESNO MB_ICONEXCLAMATION)))

(define (main)
  (let* ((info (get-driver-info))
         (version (car info))
         (dlurl (cadr info))
         (installed-version (get-installed-version)))
    (cond
     ((and version
           installed-version
           (> version installed-version)
           (= IDYES (ask-for-update (number->string installed-version)
                                    (number->string version))))
      (open-process (list path: CMD
                          arguments: (list "/c" "start" dlurl)
                          show-console: #f))))))

(main)
