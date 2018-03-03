## v0.1.0 -> v0.2.0

- Fixes
  - Fixed off-by-one error when computing the median for slope estimates

- Artifact removal procedures
  - Automatic correction now uses linear interpolation when correcting groups of
    level-shifts with a very small time span
  - All slope estimates now use the same procedure

- User interface
  - Added a feature to take screenshots of the display
  - Zooming now acts in both of the horizontal and vertical axes by default
  - The "interpolation brush" now only requires key modifiers upon releasing the mouse button
  - The colouring of level-shift highlights is now consistent when changing the view
  - Tweaked colours

- User's guide
  - Added a new section, "More on Sonomicrometry"
  - Edits

- Other
  - Changed the default 'noise threshold' from 0.12 mm to 0.10 mm
  - Optimizations
