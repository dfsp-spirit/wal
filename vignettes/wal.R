## -----------------------------------------------------------------------------
library("wal");
wal_file = system.file("extdata", "bricks.wal", package = "wal", mustWork = TRUE);
wal_image = wal::readWAL(wal_file);

## -----------------------------------------------------------------------------
dim(wal_image);

## -----------------------------------------------------------------------------
wal = wal::read.wal(wal_file);

## -----------------------------------------------------------------------------
wal$header$width;

## -----------------------------------------------------------------------------
plot(wal);

## -----------------------------------------------------------------------------
plotwal.mipmap(wal, apply_palette = wal::pal_q2(), mip_level = 1)

## ---- eval = FALSE------------------------------------------------------------
#  writeWAL("~/mytexture.wal", wal);

## ---- eval = FALSE------------------------------------------------------------
#  wal.export.to.jpeg(wal, "~/mytexture.jpg");
#  wal.export.to.png(wal, "~/mytexture.png");

## ---- eval = FALSE------------------------------------------------------------
#  wal_imported = img.to.wal(png::readPNG("~/mytexture.png"));
#  writeWAL("~/mytexture.wal", wal_imported);
#  
#  wal_imported = img.to.wal(jpeg::readJPEG("~/mytexture.jpg"));
#  writeWAL("~/mytexture.wal", wal_imported);

