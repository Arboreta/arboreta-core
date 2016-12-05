#!/bin/bash
echo -e "#!/bin/sh\ncd $PWD/src\nsbcl --noinform --end-runtime-options --load fancy-repl.cl --end-toplevel-options" '$*' > arboreta-repl.sh
chmod +x arboreta-repl.sh
