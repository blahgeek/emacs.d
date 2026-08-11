# Same as xonsh $TITLE = 'xonsh{current_job: - {}}':
# "fish" when idle, "fish - <current job>" while a command is running.
# $argv[1] is the currently running foreground command line (= xonsh current_job).
function fish_title
    if set -q argv[1]
        # only the command name, no arguments
        echo "fish - "(string split -f 1 ' ' -- $argv[1])
    else
        echo fish
    end
end
