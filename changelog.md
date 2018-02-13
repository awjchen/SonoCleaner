## v0.1.0 -> v0.2.0

- Fixes
  - Fix off-by-one error when computing the median for slope estimates

- Artifact removal procedures
  - Automatic correction now uses linear interpolation when correcting groups of
    level-shifts with a very small time span
  - All slope estimates now use the same procedure

- User interface
  - Add a feature to make screenshots of the display (supported formats: png, svg, ps,
    and pdf)
  - Zooming now acts in both of the horizontal and vertical axes by default
  - Interpolation brush: only require key modifiers upon releasing the mouse button
  - Make consistent the colouring of level-shift highlights when changing the view
  - Tweak colours

- User's guide
  - Add a new section, "More on Sonomicrometry"

- Other
  - Change default 'noise threshold' from 0.12 mm to 0.10 mm
  - Optimizations
