# Ported from xonshconf/env.xsh mkcd
function mkcd
    if test (count $argv) -ne 1
        echo 'usage: mkcd DIR' >&2
        return 1
    end
    mkdir -p -- $argv[1]; and cd -- $argv[1]
end
