(defun decimal-to-binary-string (number) 
"Convert a decimal number to its binary representation in 
string form.

Returns string form of the binary representation of number"

  (let ((poweroftwo 4096)
	(output "")) 
    (while (> poweroftwo 0)
      (if (= (/ number poweroftwo) 0)
	  (setq output (concat output "0"))
	(setq output (concat output "1"))
	(setq number (- number poweroftwo)))

      (setq poweroftwo (/ poweroftwo 2))

      (message "%s" output))
    output))

(decimal-to-binary-string 4087)

(defun count-set-bits-string (binary-string)
"Count the number of set bits (bits with value 1) in a 
binary string.  

Returns an integer value indicating the number of set 
bits in the string."

  (let ((length (length binary-string))
	(current-digit "")
	(current-digit-index 0)
	(set-bits 0))
    (while (> length 0)
      (setq current-digit (substring binary-string 
				     current-digit-index
				     (+ current-digit-index 1)))

      (if (string-equal current-digit "1")
	  (setq set-bits (1+ set-bits)))

      (setq current-digit-index (1+ current-digit-index))
      (setq length (1- length)))
    set-bits))

(defun count-set-bits (number)
"Count the number of set bits in the passed in number 
which is a base 10 integer; assumed to be >0.  

Returns an integer indicating how many bits are set."
  (let ((result 0)) 
    (while (> number 0)
      (setq number (logand number (1- number)))
      (setq result (1+ result)))
    result))

