#!/bin/bash
echo -e "#!/bin/sh\ncd $PWD/src\nsbcl --noinform --load fancy-repl.cl" > arboreta-repl.sh
chmod +x arboreta-repl.sh
