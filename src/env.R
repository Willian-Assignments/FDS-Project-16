
WORKDIR <- getwd()

FIGDIR <- file.path(WORKDIR, 'fig')
SRCDIR <- file.path(WORKDIR, 'src')

if (!dir.exists(FIGDIR)) dir.create(FIGDIR)
if (!dir.exists(SRCDIR)) dir.create(SRCDIR)
