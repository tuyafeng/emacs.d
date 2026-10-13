// launcher-icons.js --- Native app icon worker for launcher-icons.el
//
//   /usr/bin/osascript -l JavaScript launcher-icons.js PIXELS APP PNG [APP PNG]...
//
// Draws each APP's icon, as AppKit gives it (NSWorkspace iconForFile:),
// into a PIXELS-square PNG written atomically at PNG.  Arguments are only
// data.  Prints a line for each app it failed on, and goes on.

ObjC.import('AppKit');

function drawPNG(icon, pixels, output) {
  const bitmap = $.NSBitmapImageRep.alloc
    .initWithBitmapDataPlanesPixelsWidePixelsHighBitsPerSampleSamplesPerPixelHasAlphaIsPlanarColorSpaceNameBytesPerRowBitsPerPixel(
      null, pixels, pixels, 8, 4, true, false, $.NSDeviceRGBColorSpace, 0, 0);
  $.NSGraphicsContext.saveGraphicsState;
  try {
    // Assigning currentContext as a property draws nothing: call the setter.
    $.NSGraphicsContext.setCurrentContext(
      $.NSGraphicsContext.graphicsContextWithBitmapImageRep(bitmap));
    icon.drawInRectFromRectOperationFraction(
      $.NSMakeRect(0, 0, pixels, pixels), $.NSZeroRect,
      $.NSCompositingOperationCopy, 1.0);
  } finally {
    $.NSGraphicsContext.restoreGraphicsState;
  }
  const png = bitmap.representationUsingTypeProperties($.NSBitmapImageFileTypePNG, $({}));
  if (!png.writeToFileAtomically($(output), true)) throw new Error('cannot write ' + output);
}

function run(argv) {
  const pixels = parseInt(argv[0], 10);
  const failures = [];
  for (let i = 1; i + 1 < argv.length; i += 2) {
    const app = argv[i];
    try {
      // AppKit gives a generic icon for a missing file.
      if (!$.NSFileManager.defaultManager.fileExistsAtPath($(app)))
        throw new Error('no such application');
      drawPNG($.NSWorkspace.sharedWorkspace.iconForFile($(app)), pixels, argv[i + 1]);
    } catch (error) {
      failures.push(app + ': ' + error.message);
    }
  }
  return failures.join('\n');
}
