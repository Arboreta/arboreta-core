#!/bin/bash
echo -e "#!/bin/sh\ncd $PWD/src\nsbcl --noinform --end-runtime-options --load fancy-repl.cl --end-toplevel-options" '$*' > arboreta-repl.sh
chmod +x arboreta-repl.sh

# echo -e "#!/bin/sh\ncd $PWD/src\nsbcl --noinform --end-runtime-options --load irc.cl --end-toplevel-options" '$*' > arboreta-irc.sh
# chmod +x arboreta-irc.sh
