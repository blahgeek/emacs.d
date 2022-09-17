;;; abbrev-skeletons.el --- Definition of abbrev skeletons -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(with-eval-after-load 'cc-mode
  (my/define-abbrev-skeleton cc-main "$main" c++-mode-abbrev-table
                             nil
                             "int main(int argc, char *argv[]) {" \n
                             _ \n
                             "return 0;" \n
                             "}"))

;;; abbrev-skeletons.el ends here
