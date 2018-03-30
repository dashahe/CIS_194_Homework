(define (letterwords l sent)
  (keep (lambda (wd) (member l wd)) sent))