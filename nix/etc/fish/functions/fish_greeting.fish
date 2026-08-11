# Ported from xonshconf/greeting.py
function fish_greeting
    echo
    echo 'Machine:  '(uname -srn)
    echo 'Date:     '(date)
    echo 'Uptime:   '(uptime | string trim)
    echo
end
