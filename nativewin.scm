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


;; Functions:
;;
;; (message-box message title configuration)
;;
;; Calls Win32 API MessageBox() function with following parameters:
;; message       - message text to be shown in the message dialog window
;; title         - title for the window
;; configuration - OR'ed configuration parameters for icon and buttons
;; return value  - what button has been pressed
;;
;; Example:
;;   (message-box "Hello, World!", "Window Title", (bitwise-ior MB_OK MB_ICONINFORMATION))
;;
;; (winhttp-check-platform)
;;
;; Returns #t if the platform supports WinHTTP API, otherwise #f.
;;
;; (winhttp-get hostname path ssl?)
;;
;; hostname     - hostname of the server (e.g. "www.example.com")
;; path         - path for the resource, #f can be used for "/"
;; ssl?         - #t if encryption shall be used (HTTPS port 443 will be used)
;;                #f if encryption is not required (HTTP port 80 will be used)
;; return value - string containing the resource (e.g. the source code of a web page)


(c-declare
#<<c-declare-end
  #include <windows.h>
  #include <winhttp.h>
c-declare-end
)

(define MB_OK              #x00000000)
(define MB_YESNO           #x00000004)
(define MB_ICONEXCLAMATION #x00000030)
(define MB_ICONINFORMATION #x00000040)
(define MB_ICONQUESTION    #x00000020)
(define MB_ICONSTOP        #x00000010)

(define IDOK       01)
(define IDCANCEL   02)
(define IDABORT    03)
(define IDRETRY    04)
(define IDIGNORE   05)
(define IDYES      06)
(define IDNO       07)
(define IDTRYAGAIN 10)
(define IDCONTINUE 11)

(define message-box
  (c-lambda (char-string char-string unsigned-int32)
            int32
            "___return(MessageBox(NULL, ___arg1, ___arg2, ___arg3));"))

(define winhttp-check-platform
  (c-lambda ()
            bool
            "___return(WinHttpCheckPlatform() ? 1 : 0);"))

(define winhttp-get
  (c-lambda (UTF-16-string UTF-16-string bool)
            char-string
#<<c-code-end
  LPSTR result = malloc(1);
  result[0] = '\0';
  DWORD resultIndex = 0;
  DWORD dwSize = 0;
  DWORD dwDownloaded = 0;
  LPSTR pszOutBuffer;
  BOOL bResults = FALSE;
  HINTERNET hSession = NULL,
            hConnect = NULL,
            hRequest = NULL;

  hSession = WinHttpOpen(L"Gambit Scheme WinHTTP Library/1.0",
                         WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
                         WINHTTP_NO_PROXY_NAME,
                         WINHTTP_NO_PROXY_BYPASS,
                         0);

  if (hSession) hConnect = WinHttpConnect(hSession,
                                          ___arg1,
                                          ___arg3 ? 443 : 80,
                                          0);

  if (hConnect) hRequest = WinHttpOpenRequest(hConnect,
                                              L"GET",
                                              ___arg2,
                                              NULL,
                                              WINHTTP_NO_REFERER,
                                              WINHTTP_DEFAULT_ACCEPT_TYPES,
                                              ___arg3 ? WINHTTP_FLAG_SECURE : 0);

  if (hRequest) bResults = WinHttpSendRequest(hRequest,
                                              WINHTTP_NO_ADDITIONAL_HEADERS,
                                              0,
                                              WINHTTP_NO_REQUEST_DATA, 0, 0, 0);

  if (bResults) bResults = WinHttpReceiveResponse(hRequest, NULL);

  if (bResults) {
    do {
      dwSize = 0;
      WinHttpQueryDataAvailable(hRequest, &dwSize);
      pszOutBuffer = malloc(dwSize + 1);
      if (!pszOutBuffer) {
        dwSize = 0;
      } else {
        ZeroMemory(pszOutBuffer, dwSize + 1);
        if (WinHttpReadData(hRequest, (LPVOID) pszOutBuffer, dwSize, &dwDownloaded)) {
          result = realloc(result, resultIndex + dwSize + 1);
          ZeroMemory(&result[resultIndex], dwSize + 1);
          strcpy(&result[resultIndex], pszOutBuffer);
          resultIndex += dwSize;
        }
        free(pszOutBuffer);
      }
    } while (dwSize > 0);
  }

  if (hRequest) WinHttpCloseHandle(hRequest);
  if (hConnect) WinHttpCloseHandle(hConnect);
  if (hSession) WinHttpCloseHandle(hSession);

  // Gambit documentation states that the garbage collection will handle
  // the allocated memory of the foreign object (in this case char-string).
  // So, calling free(result) is not necessary after this function returns.
  
  ___return(result);
c-code-end
))
