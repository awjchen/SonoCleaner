-- The Glade file defining the GUI
--
-- We store the Glade file as a string so that it may be included in the binary
-- itself. I'd prefer not to do things this way, but I don't know how else to do
-- this.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Controller.Glade
  ( builderString
  ) where

import qualified Data.Text         as T
import           Text.RawString.QQ (r)

builderString :: T.Text
builderString = [r|
<?xml version="1.0" encoding="UTF-8"?>
<!-- Generated with glade 3.22.1 -->
<interface>
  <requires lib="gtk+" version="3.10"/>
  <object class="GtkAdjustment" id="jumpToleranceAdjustment">
    <property name="lower">0.029999999999999999</property>
    <property name="upper">0.98999999999999999</property>
    <property name="value">0.71999999999999997</property>
    <property name="step_increment">0.029999999999999999</property>
    <property name="page_increment">0.14999999999999999</property>
  </object>
  <object class="GtkAdjustment" id="multipleOffsetAdjustment">
    <property name="lower">-1</property>
    <property name="upper">1</property>
    <property name="step_increment">0.01</property>
    <property name="page_increment">0.050000000000000003</property>
  </object>
  <object class="GtkAdjustment" id="singleOffsetAdjustment">
    <property name="lower">-1</property>
    <property name="upper">1</property>
    <property name="step_increment">0.01</property>
    <property name="page_increment">0.050000000000000003</property>
  </object>
  <object class="GtkAdjustment" id="thresholdAdjustment">
    <property name="lower">0.01</property>
    <property name="upper">0.33000000000000002</property>
    <property name="value">0.23999999999999999</property>
    <property name="step_increment">0.01</property>
    <property name="page_increment">0.050000000000000003</property>
  </object>
  <object class="GtkAdjustment" id="windowSizeAdjustment">
    <property name="upper">20</property>
    <property name="step_increment">1</property>
    <property name="page_increment">20</property>
  </object>
  <object class="GtkWindow" id="controllerWindow">
    <property name="can_focus">False</property>
    <child type="titlebar">
      <placeholder/>
    </child>
    <child>
      <object class="GtkBox">
        <property name="visible">True</property>
        <property name="can_focus">False</property>
        <property name="orientation">vertical</property>
        <child>
          <object class="GtkEventBox" id="imageEventBox">
            <property name="can_focus">False</property>
            <property name="halign">start</property>
            <property name="valign">start</property>
            <child>
              <object class="GtkImage" id="image">
                <property name="can_focus">False</property>
                <property name="stock">gtk-missing-image</property>
              </object>
            </child>
          </object>
          <packing>
            <property name="expand">False</property>
            <property name="fill">True</property>
            <property name="position">0</property>
          </packing>
        </child>
        <child>
          <object class="GtkBox" id="controllerWindowBox">
            <property name="visible">True</property>
            <property name="can_focus">False</property>
            <property name="spacing">8</property>
            <child>
              <object class="GtkBox" id="zoomButtonsBox">
                <property name="visible">True</property>
                <property name="can_focus">False</property>
                <property name="orientation">vertical</property>
                <child>
                  <object class="GtkButton" id="mainFullViewButton">
                    <property name="label" translatable="yes">Full view</property>
                    <property name="visible">True</property>
                    <property name="can_focus">True</property>
                    <property name="receives_default">True</property>
                    <property name="tooltip_text" translatable="yes">Set the view to fit the trace in the display.

[Ctrl + r]</property>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">0</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkButton" id="mainFullViewXButton">
                    <property name="label" translatable="yes">Full x-axis</property>
                    <property name="visible">True</property>
                    <property name="can_focus">True</property>
                    <property name="receives_default">True</property>
                    <property name="tooltip_text" translatable="yes">Set the x-axis to fit the length of the trace.

[Ctrl + x]</property>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">1</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkButton" id="mainFullViewYButton">
                    <property name="label" translatable="yes">Full y-axis</property>
                    <property name="visible">True</property>
                    <property name="can_focus">True</property>
                    <property name="receives_default">True</property>
                    <property name="tooltip_text" translatable="yes">Set the y-axis to fit the height of the trace.

[Ctrl + y]</property>
                  </object>
                  <packing>
                    <property name="expand">False</property>
                    <property name="fill">True</property>
                    <property name="position">2</property>
                  </packing>
                </child>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">0</property>
              </packing>
            </child>
            <child>
              <object class="GtkNotebook" id="notebook">
                <property name="visible">True</property>
                <property name="can_focus">True</property>
                <property name="show_tabs">False</property>
                <property name="show_border">False</property>
                <child>
                  <object class="GtkBox" id="mainRowsBox">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="orientation">vertical</property>
                    <child>
                      <object class="GtkBox" id="mainButtonsBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="spacing">8</property>
                        <child>
                          <object class="GtkBox" id="mainFileBox">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="orientation">vertical</property>
                            <child>
                              <object class="GtkButton" id="openButton">
                                <property name="label" translatable="yes">Open</property>
                                <property name="visible">True</property>
                                <property name="can_focus">False</property>
                                <property name="focus_on_click">False</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">Open a .ssa file.

[Ctrl + o]</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">0</property>
                              </packing>
                            </child>
                            <child>
                              <object class="GtkButton" id="saveButton">
                                <property name="label" translatable="yes">Save</property>
                                <property name="visible">True</property>
                                <property name="can_focus">False</property>
                                <property name="focus_on_click">False</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">Save to a .ssa file. Will prompt for confirmation before overwriting any files.

[Ctrl + s]</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">1</property>
                              </packing>
                            </child>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkBox" id="mainTraceNavigationBox">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="orientation">vertical</property>
                            <child>
                              <object class="GtkButton" id="prevTraceButton">
                                <property name="label" translatable="yes">Prev trace</property>
                                <property name="visible">True</property>
                                <property name="can_focus">True</property>
                                <property name="focus_on_click">False</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">[Ctrl + b]</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">0</property>
                              </packing>
                            </child>
                            <child>
                              <object class="GtkButton" id="nextTraceButton">
                                <property name="label" translatable="yes">Next trace</property>
                                <property name="visible">True</property>
                                <property name="can_focus">False</property>
                                <property name="focus_on_click">False</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">[Ctrl + f]</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">1</property>
                              </packing>
                            </child>
                            <child>
                              <object class="GtkButton" id="twinTraceButton">
                                <property name="label" translatable="yes">Twin trace</property>
                                <property name="visible">True</property>
                                <property name="can_focus">True</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">Switch between 'twin' traces (e.g. TRX01:02 and TRX02:01).

[Ctrl + t]</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">2</property>
                              </packing>
                            </child>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">1</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkBox" id="mainUndoBox">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="orientation">vertical</property>
                            <child>
                              <object class="GtkButton" id="undoButton">
                                <property name="label" translatable="yes">Undo</property>
                                <property name="visible">True</property>
                                <property name="can_focus">True</property>
                                <property name="focus_on_click">False</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">[Ctrl + z]</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">0</property>
                              </packing>
                            </child>
                            <child>
                              <object class="GtkButton" id="redoButton">
                                <property name="label" translatable="yes">Redo</property>
                                <property name="visible">True</property>
                                <property name="can_focus">True</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">[Ctrl + Shift + z]</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">1</property>
                              </packing>
                            </child>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">2</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkBox" id="mainActionsBox">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="orientation">vertical</property>
                            <child>
                              <object class="GtkButton" id="autoButton">
                                <property name="label" translatable="yes">Auto</property>
                                <property name="visible">True</property>
                                <property name="can_focus">True</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">Apply automatic correction of level-shifts.

[a]</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">0</property>
                              </packing>
                            </child>
                            <child>
                              <object class="GtkButton" id="labellingButton">
                                <property name="label" translatable="yes">Labelling</property>
                                <property name="visible">True</property>
                                <property name="can_focus">True</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">Adjust parameters for the labelling of level-shifts.

[b]</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">1</property>
                              </packing>
                            </child>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">3</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkBox" id="mainMiscBox">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="orientation">vertical</property>
                            <child>
                              <object class="GtkButton" id="viewButton">
                                <property name="label" translatable="yes">Compare</property>
                                <property name="visible">True</property>
                                <property name="can_focus">True</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">Superimpose other traces for comparison.

[v]</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">0</property>
                              </packing>
                            </child>
                            <child>
                              <object class="GtkButton" id="cropButton">
                                <property name="label" translatable="yes">Crop</property>
                                <property name="visible">True</property>
                                <property name="can_focus">True</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">Restrict the view and manipulation of traces to a time interval.

[c]</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">1</property>
                              </packing>
                            </child>
                            <child>
                              <object class="GtkButton" id="qualityButton">
                                <property name="label" translatable="yes">Quality</property>
                                <property name="visible">True</property>
                                <property name="can_focus">True</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">Annotate traces with quality ratings.

[q]</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">2</property>
                              </packing>
                            </child>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">4</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkBox" id="mainScreenshotBox">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="orientation">vertical</property>
                            <child>
                              <object class="GtkButton" id="screenshotButton">
                                <property name="label" translatable="yes">Screenshot</property>
                                <property name="visible">True</property>
                                <property name="can_focus">True</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">Take a screenshot of the current view.

[s]</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">0</property>
                              </packing>
                            </child>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">5</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkBox" id="mainHelpBox">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="orientation">vertical</property>
                            <child>
                              <object class="GtkImage" id="mainHelpImage">
                                <property name="visible">True</property>
                                <property name="can_focus">False</property>
                                <property name="tooltip_text" translatable="yes">Display control

- Center display at point: left mouse button click
- Zoom in/out: mouse wheel scroll
- Zoom in/out horizontally: Ctrl + mouse wheel scroll
- Zoom in/out vertically: Shift + mouse wheel scroll

Level-shift selection

- Select nearest level-shift: right mouse button click
- Select all level-shifts in a time interval: right mouse button click and drag

Interpolation brush

- Replace points below a specified line: Control + right mouse button click and drag
- Replace points above a specified line: Shift + right mouse button click and drag</property>
                                <property name="stock">gtk-dialog-question</property>
                                <property name="icon_size">3</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">0</property>
                              </packing>
                            </child>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">6</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkBox" id="mainOptionsBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <child>
                          <placeholder/>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                  </object>
                </child>
                <child type="tab">
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">Main</property>
                  </object>
                  <packing>
                    <property name="tab_fill">False</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="autoSettingsBox">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="spacing">8</property>
                    <child>
                      <object class="GtkGrid" id="autoParametersGrid">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <property name="row_spacing">4</property>
                        <property name="column_spacing">4</property>
                        <child>
                          <object class="GtkLabel">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="tooltip_text" translatable="yes">Level-shifts are eliminated by the automated procedure only if they can be matched with others in a suitable group. Matches are eliminated in order of increasing time span: increasing the match level allows matches over longer time spans.

[g, f, b, Shift + g, Shift + f, Shift + b]</property>
                            <property name="halign">start</property>
                            <property name="label" translatable="yes">Match level</property>
                          </object>
                          <packing>
                            <property name="left_attach">0</property>
                            <property name="top_attach">0</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkSpinButton" id="matchLevelSpinButton">
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="tooltip_text" translatable="yes">Level-shifts are eliminated by the automated procedure only if they can be matched with others in a suitable group. Matches are eliminated in order of increasing time span: increasing the match level allows matches over longer time spans.

[g, f, b, Shift + g, Shift + f, Shift + b]</property>
                            <property name="text" translatable="yes">8</property>
                            <property name="input_purpose">number</property>
                            <property name="adjustment">windowSizeAdjustment</property>
                            <property name="numeric">True</property>
                            <property name="update_policy">if-valid</property>
                            <property name="value">4</property>
                          </object>
                          <packing>
                            <property name="left_attach">1</property>
                            <property name="top_attach">0</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkBox" id="autoOptionsBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <property name="spacing">4</property>
                        <child>
                          <object class="GtkBox" id="autoFinalBox">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="spacing">4</property>
                            <child>
                              <object class="GtkButton" id="autoCancelButton">
                                <property name="label" translatable="yes">Cancel</property>
                                <property name="visible">True</property>
                                <property name="can_focus">True</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">[Tab, Escape]</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">0</property>
                              </packing>
                            </child>
                            <child>
                              <object class="GtkButton" id="autoApplyButton">
                                <property name="label" translatable="yes">Apply</property>
                                <property name="visible">True</property>
                                <property name="can_focus">True</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">[Space]</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">1</property>
                              </packing>
                            </child>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkBox" id="autoHelpBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <child>
                          <object class="GtkImage" id="autoHelpImage">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="tooltip_text" translatable="yes">Automatic correction of level-shifts

- Increase the 'Match level' to progressively match and eliminate level-shifts.
- This automatic procedure is not expected to gracefully handle all cases. Therefore, observe the changes it makes for errors.
- Apply the automated procedure at the highest 'Match level' at which you observe no errors, then make manual corrections to address the artifacts responsible for the errors.</property>
                            <property name="stock">gtk-dialog-question</property>
                            <property name="icon_size">3</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">2</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="position">1</property>
                  </packing>
                </child>
                <child type="tab">
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">Auto</property>
                  </object>
                  <packing>
                    <property name="position">1</property>
                    <property name="tab_fill">False</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="singleSettingsBox">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="spacing">8</property>
                    <child>
                      <object class="GtkGrid" id="singleParametersGrid">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="row_spacing">4</property>
                        <property name="column_spacing">4</property>
                        <child>
                          <object class="GtkComboBoxText" id="singleHoldComboBox">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="tooltip_text" translatable="yes">Select the half of the trace should be preserved when changing the height of a level-shift.

[q, w]</property>
                            <property name="active">0</property>
                            <items>
                              <item translatable="yes">Left</item>
                              <item translatable="yes">Right</item>
                            </items>
                          </object>
                          <packing>
                            <property name="left_attach">1</property>
                            <property name="top_attach">1</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkLabel">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="tooltip_text" translatable="yes">Select the half of the trace should be preserved when changing the height of a level-shift.

[q, w]</property>
                            <property name="label" translatable="yes">Hold</property>
                          </object>
                          <packing>
                            <property name="left_attach">0</property>
                            <property name="top_attach">1</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkLabel">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="tooltip_text" translatable="yes">Apply a displacement to the height of a level-shift.

[f, b, Shift + f, Shift + b]</property>
                            <property name="label" translatable="yes">Manual
adjustment</property>
                          </object>
                          <packing>
                            <property name="left_attach">0</property>
                            <property name="top_attach">0</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkSpinButton" id="singleOffsetSpinButton">
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="tooltip_text" translatable="yes">Apply a displacement to the height of a level-shift.

[f, b, Shift + f, Shift + b]</property>
                            <property name="input_purpose">number</property>
                            <property name="adjustment">singleOffsetAdjustment</property>
                            <property name="digits">2</property>
                            <property name="numeric">True</property>
                            <property name="update_policy">if-valid</property>
                          </object>
                          <packing>
                            <property name="left_attach">1</property>
                            <property name="top_attach">0</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkBox" id="singleOptionsBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <child>
                          <object class="GtkRadioButton" id="singleIgnoreRadioButton">
                            <property name="label" translatable="yes">Ignore</property>
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="focus_on_click">False</property>
                            <property name="receives_default">False</property>
                            <property name="tooltip_text" translatable="yes">Make no change.

[g]</property>
                            <property name="active">True</property>
                            <property name="draw_indicator">True</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkRadioButton" id="singleZeroRadioButton">
                            <property name="label" translatable="yes">Zero</property>
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="focus_on_click">False</property>
                            <property name="receives_default">False</property>
                            <property name="tooltip_text" translatable="yes">Set the height of the level-shift to zero. This option can be used as a default when the trace is too noisy for a clean 'Slope fit'.

[z]</property>
                            <property name="draw_indicator">True</property>
                            <property name="group">singleIgnoreRadioButton</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">1</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkRadioButton" id="singleSlopeFitRadioButton">
                            <property name="label" translatable="yes">Slope fit</property>
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="focus_on_click">False</property>
                            <property name="receives_default">False</property>
                            <property name="tooltip_text" translatable="yes">Set the height of the level-shift equal to an estimate of the slope of the surrounding trace.

[s]</property>
                            <property name="draw_indicator">True</property>
                            <property name="group">singleIgnoreRadioButton</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">2</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkBox" id="singleFinalBox">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="spacing">4</property>
                            <child>
                              <object class="GtkButton" id="singleCancelButton">
                                <property name="label" translatable="yes">Cancel</property>
                                <property name="visible">True</property>
                                <property name="can_focus">True</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">[Tab, Escape]</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">0</property>
                              </packing>
                            </child>
                            <child>
                              <object class="GtkButton" id="singleApplyButton">
                                <property name="label" translatable="yes">Apply</property>
                                <property name="visible">True</property>
                                <property name="can_focus">True</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">[Space]</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">1</property>
                              </packing>
                            </child>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">3</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkBox" id="singleHelpBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <child>
                          <object class="GtkImage" id="singleHelpImage">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="tooltip_text" translatable="yes">Manual correction of an individual level-shift

- Select one of the options on the right to preview a correction.
- Make fine adjustments with the 'Manual adjustment' (use sparingly).
- Select the side of the trace that should be preserved by setting the 'Hold'.
- Click the 'Apply' button when satisfied, or the 'Cancel' button otherwise.</property>
                            <property name="stock">gtk-dialog-question</property>
                            <property name="icon_size">3</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">2</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="position">2</property>
                  </packing>
                </child>
                <child type="tab">
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">Single</property>
                  </object>
                  <packing>
                    <property name="position">2</property>
                    <property name="tab_fill">False</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="multipleSettingsBox">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="spacing">8</property>
                    <child>
                      <object class="GtkGrid" id="multipleParametersGrid">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="row_spacing">4</property>
                        <property name="column_spacing">4</property>
                        <child>
                          <object class="GtkLabel">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="tooltip_text" translatable="yes">Add a distance offset to the portion of the trace spanned by the group of level-shifts.

[f, b, Shift + f, Shift + b]</property>
                            <property name="label" translatable="yes">Manual
adjustment</property>
                          </object>
                          <packing>
                            <property name="left_attach">0</property>
                            <property name="top_attach">0</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkSpinButton" id="multipleOffsetSpinButton">
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="tooltip_text" translatable="yes">Add a distance offset to the portion of the trace spanned by the group of level-shifts.

[f, b, Shift + f, Shift + b]</property>
                            <property name="input_purpose">number</property>
                            <property name="adjustment">multipleOffsetAdjustment</property>
                            <property name="digits">2</property>
                            <property name="numeric">True</property>
                            <property name="update_policy">if-valid</property>
                          </object>
                          <packing>
                            <property name="left_attach">1</property>
                            <property name="top_attach">0</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkBox" id="multipleOptionsBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <child>
                          <object class="GtkRadioButton" id="multipleIgnoreRadioButton">
                            <property name="label" translatable="yes">Ignore</property>
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="receives_default">False</property>
                            <property name="tooltip_text" translatable="yes">Make no change.

[g]</property>
                            <property name="active">True</property>
                            <property name="draw_indicator">True</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkRadioButton" id="multipleLineRadioButton">
                            <property name="label" translatable="yes">Line</property>
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="receives_default">False</property>
                            <property name="tooltip_text" translatable="yes">Replace the portion of the trace spanned by a group of level-shifts by a linear interpolation. This option is useful for when the data in a level-shifted interval is nonsensical.

[r]</property>
                            <property name="active">True</property>
                            <property name="draw_indicator">True</property>
                            <property name="group">multipleIgnoreRadioButton</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">1</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkRadioButton" id="multipleCancelRadioButton">
                            <property name="label" translatable="yes">Sum</property>
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="receives_default">False</property>
                            <property name="tooltip_text" translatable="yes">Redistribute all of the offsets of the level-shifts to the first. That is, set the heights of all the level-shifts to zero, except for the first, which is set to the sum of the heights.

[s]</property>
                            <property name="active">True</property>
                            <property name="draw_indicator">True</property>
                            <property name="group">multipleIgnoreRadioButton</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">2</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkBox" id="multipleFinalBox">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="spacing">4</property>
                            <child>
                              <object class="GtkButton" id="multipleCancelButton">
                                <property name="label" translatable="yes">Cancel</property>
                                <property name="visible">True</property>
                                <property name="can_focus">True</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">[Tab, Escape]</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">0</property>
                              </packing>
                            </child>
                            <child>
                              <object class="GtkButton" id="multipleApplyButton">
                                <property name="label" translatable="yes">Apply</property>
                                <property name="visible">True</property>
                                <property name="can_focus">True</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">[Space]</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">1</property>
                              </packing>
                            </child>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">3</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkBox" id="multipleHelpBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <child>
                          <object class="GtkImage" id="multipleHelpImage">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="tooltip_text" translatable="yes">Manual correction of a group of level-shifts

- Select one of the options on the right to preview a correction.
- Make fine adjustments with the 'Manual adjustment' (use sparingly).
- Click the 'Apply' button when satisfied, or the 'Cancel' button otherwise.</property>
                            <property name="stock">gtk-dialog-question</property>
                            <property name="icon_size">3</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">2</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="position">3</property>
                  </packing>
                </child>
                <child type="tab">
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">Multiple</property>
                  </object>
                  <packing>
                    <property name="position">3</property>
                    <property name="tab_fill">False</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="labelSettingsBox">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="spacing">8</property>
                    <child>
                      <object class="GtkGrid" id="labelParametersGrid">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <property name="row_spacing">4</property>
                        <property name="column_spacing">4</property>
                        <child>
                          <object class="GtkSpinButton" id="noiseThresholdSpinButton">
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="tooltip_text" translatable="yes">The maximum change in distance over one timestep that can be attributed to random noise.

The automated procedure will only match level-shifts if their difference is less than the noise threshold. The labelling procedure depends similarly on the noise threshold.

[Up, Down; or j, k]</property>
                            <property name="input_purpose">number</property>
                            <property name="adjustment">thresholdAdjustment</property>
                            <property name="digits">2</property>
                            <property name="numeric">True</property>
                            <property name="update_policy">if-valid</property>
                            <property name="value">0.02</property>
                          </object>
                          <packing>
                            <property name="left_attach">1</property>
                            <property name="top_attach">1</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkLabel" id="noiseThresholdLabel">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="tooltip_text" translatable="yes">The maximum change in distance over one timestep that can be attributed to random noise.

The automated procedure will only match level-shifts if their difference is less than the noise threshold. The labelling procedure depends similarly on the noise threshold.

[Up, Down; or j, k]</property>
                            <property name="halign">start</property>
                            <property name="label" translatable="yes">Noise threshold
(mm)</property>
                          </object>
                          <packing>
                            <property name="left_attach">0</property>
                            <property name="top_attach">1</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkLabel" id="levelShiftThresholdLabel">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="tooltip_text" translatable="yes">The threshold above which a change in distance over a single time step is considered as a level-shift.

[Shift+Up, Shift+Down; or Shift +j, Shift + k]</property>
                            <property name="halign">start</property>
                            <property name="label" translatable="yes">Level-shift threshold
(mm)</property>
                          </object>
                          <packing>
                            <property name="left_attach">0</property>
                            <property name="top_attach">0</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkSpinButton" id="levelShiftThresholdSpinButton">
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="tooltip_text" translatable="yes">The threshold above which a change in distance over a single time step is considered as a level-shift.

[Shift+Up, Shift+Down; or Shift +j, Shift + k]</property>
                            <property name="input_purpose">number</property>
                            <property name="adjustment">jumpToleranceAdjustment</property>
                            <property name="digits">2</property>
                            <property name="numeric">True</property>
                            <property name="update_policy">if-valid</property>
                            <property name="value">0.02</property>
                          </object>
                          <packing>
                            <property name="left_attach">1</property>
                            <property name="top_attach">0</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkBox" id="labelOptionsBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <property name="spacing">4</property>
                        <child>
                          <object class="GtkButton" id="defaultParametersButton">
                            <property name="label" translatable="yes">Reset parameters</property>
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="receives_default">True</property>
                            <property name="tooltip_text" translatable="yes">Set the labelling parameters to their default values.

[r]</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkBox" id="labelFinalBox">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="orientation">vertical</property>
                            <child>
                              <object class="GtkButton" id="labelBackButton">
                                <property name="label" translatable="yes">Back</property>
                                <property name="visible">True</property>
                                <property name="can_focus">True</property>
                                <property name="receives_default">True</property>
                                <property name="tooltip_text" translatable="yes">[Tab, Escape]</property>
                                <property name="halign">start</property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">0</property>
                              </packing>
                            </child>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">1</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkBox" id="labelHelpBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <child>
                          <object class="GtkImage" id="labelHelpImage">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="tooltip_text" translatable="yes">Level-shift labelling parameters

- The default level-shift labelling parameters should suffice for most traces, but the parameters can be adjusted here if needed.
- As a rule of thumb, the 'Level-shift threshold' should be at least three times the size of the 'Noise threshold'.</property>
                            <property name="stock">gtk-dialog-question</property>
                            <property name="icon_size">3</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">2</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="position">4</property>
                  </packing>
                </child>
                <child type="tab">
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">Label</property>
                  </object>
                  <packing>
                    <property name="position">4</property>
                    <property name="tab_fill">False</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="viewSettingsBox">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="spacing">9</property>
                    <child>
                      <object class="GtkBox" id="viewTraceSelectionBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <child>
                          <object class="GtkCheckButton" id="showReplicateTracesCheckButton">
                            <property name="label" translatable="yes">Show replicate traces</property>
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="receives_default">False</property>
                            <property name="tooltip_text" translatable="yes">Show other traces for comparison:
(1) the current trace, but in its original form without any chages (in grey), and
(2) the 'twin' trace (in blue).

[r]</property>
                            <property name="draw_indicator">True</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkBox" id="viewCustomTraceBox">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <child>
                              <object class="GtkLabel">
                                <property name="visible">True</property>
                                <property name="can_focus">False</property>
                                <property name="tooltip_text" translatable="yes">Select any trace in the file to superimpose on the display</property>
                                <property name="label" translatable="yes">Reference trace: </property>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">0</property>
                              </packing>
                            </child>
                            <child>
                              <object class="GtkComboBoxText" id="referenceTraceComboBoxText">
                                <property name="visible">True</property>
                                <property name="can_focus">False</property>
                                <property name="tooltip_text" translatable="yes">Select any trace in the file to superimpose on the display</property>
                                <property name="active">0</property>
                                <items>
                                  <item translatable="yes">None</item>
                                </items>
                              </object>
                              <packing>
                                <property name="expand">False</property>
                                <property name="fill">True</property>
                                <property name="position">1</property>
                              </packing>
                            </child>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">1</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkBox" id="viewFinalBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <child>
                          <object class="GtkButton" id="viewBackButton">
                            <property name="label" translatable="yes">Back</property>
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="receives_default">True</property>
                            <property name="tooltip_text" translatable="yes">[Tab, Escape]</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkBox" id="viewHelpBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <child>
                          <object class="GtkImage" id="viewHelpImage">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="tooltip_text" translatable="yes">Compare traces

- From here one can toggle the visibility of other traces.
- Comparison to these other traces may be used to check the validity of one's corrections.</property>
                            <property name="stock">gtk-dialog-question</property>
                            <property name="icon_size">3</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">2</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="position">5</property>
                  </packing>
                </child>
                <child type="tab">
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">View</property>
                  </object>
                  <packing>
                    <property name="position">5</property>
                    <property name="tab_fill">False</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="cropSettingsBox">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="spacing">8</property>
                    <child>
                      <object class="GtkBox" id="cropActionsBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <child>
                          <object class="GtkButton" id="applyCropButton">
                            <property name="label" translatable="yes">Crop to selection</property>
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="receives_default">True</property>
                            <property name="tooltip_text" translatable="yes">[c]</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkButton" id="applyUncropButton">
                            <property name="label" translatable="yes">Uncrop</property>
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="receives_default">True</property>
                            <property name="tooltip_text" translatable="yes">[b]</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">1</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkBox" id="cropFinalBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <child>
                          <object class="GtkButton" id="cropBackButton">
                            <property name="label" translatable="yes">Back</property>
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="receives_default">True</property>
                            <property name="tooltip_text" translatable="yes">[Tab, Escape]</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkBox" id="cropHelpBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <child>
                          <object class="GtkImage" id="cropHelpImage">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="tooltip_text" translatable="yes">Cropping

- Here one can select a time interval to which the trace should be restricted.
- To select a time interval, click and drag with the right mouse button to select the desired region, then click the 'Crop to selection' button. 
- Cropping can be undone with the 'Uncrop' button; no data is ever deleted by cropping.
- Data points outside the cropping time interval will be excluded when saving the data to a file.</property>
                            <property name="stock">gtk-dialog-question</property>
                            <property name="icon_size">3</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">2</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="position">6</property>
                  </packing>
                </child>
                <child type="tab">
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">Crop</property>
                  </object>
                  <packing>
                    <property name="position">6</property>
                    <property name="tab_fill">False</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="qualityBox">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="spacing">8</property>
                    <child>
                      <object class="GtkBox" id="qualityActionBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <child>
                          <object class="GtkButton" id="qualityGoodButton">
                            <property name="label" translatable="yes">Good</property>
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="receives_default">True</property>
                            <property name="tooltip_text" translatable="yes">[g]</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkButton" id="qualityModerateButton">
                            <property name="label" translatable="yes">Moderate</property>
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="receives_default">True</property>
                            <property name="tooltip_text" translatable="yes">[d]</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">1</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkButton" id="qualityBadButton">
                            <property name="label" translatable="yes">Bad</property>
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="receives_default">True</property>
                            <property name="tooltip_text" translatable="yes">[b]</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">2</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkBox" id="qualityFinalBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <child>
                          <object class="GtkButton" id="qualityBackButton">
                            <property name="label" translatable="yes">Back</property>
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="receives_default">True</property>
                            <property name="tooltip_text" translatable="yes">[Tab, Escape]</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkBox" id="qualityHelpBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <child>
                          <object class="GtkImage" id="qualityHelpImage">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="tooltip_text" translatable="yes">Trace quality

- Here one can annotate the current trace with a quality rating.
- The ratings 'Good', 'Moderate', and 'Bad' are meant to be interpreted for humans or other programs; they have no meaning to this program and do not affect data processing.
- 'Moderate' traces are indicated by a blue background, 'Bad' by red, and 'Good' by grey.
- These trace quality ratings are loaded and saved alongside the .ssa file in an auxillary CSV file with a '.quality' suffix.</property>
                            <property name="stock">gtk-dialog-question</property>
                            <property name="icon_size">3</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">2</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="position">7</property>
                  </packing>
                </child>
                <child type="tab">
                  <object class="GtkLabel" id="qualityPage">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="label" translatable="yes">Quality</property>
                  </object>
                  <packing>
                    <property name="position">7</property>
                    <property name="tab_fill">False</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox" id="screenshotSettingsBox">
                    <property name="visible">True</property>
                    <property name="can_focus">False</property>
                    <property name="spacing">8</property>
                    <child>
                      <object class="GtkGrid" id="screenshotParametersGrid">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="row_spacing">8</property>
                        <property name="column_spacing">8</property>
                        <child>
                          <object class="GtkLabel" id="screenshotFileFormatButton">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="label" translatable="yes">File format</property>
                          </object>
                          <packing>
                            <property name="left_attach">0</property>
                            <property name="top_attach">0</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkComboBoxText" id="screenshotFileFormatComboBoxText">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="active">0</property>
                            <items>
                              <item translatable="yes">PNG</item>
                              <item translatable="yes">SVG</item>
                              <item translatable="yes">PS</item>
                              <item translatable="yes">PDF</item>
                            </items>
                          </object>
                          <packing>
                            <property name="left_attach">1</property>
                            <property name="top_attach">0</property>
                          </packing>
                        </child>
                        <child>
                          <object class="GtkButton" id="screenshotSaveButton">
                            <property name="label" translatable="yes">Take screenshot</property>
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="receives_default">True</property>
                            <property name="tooltip_text" translatable="yes">[s]</property>
                          </object>
                          <packing>
                            <property name="left_attach">0</property>
                            <property name="top_attach">1</property>
                            <property name="width">2</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkBox" id="screenshotFinalBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <child>
                          <object class="GtkButton" id="screenshotBackButton">
                            <property name="label" translatable="yes">Back</property>
                            <property name="visible">True</property>
                            <property name="can_focus">True</property>
                            <property name="receives_default">True</property>
                            <property name="tooltip_text" translatable="yes">[Tab, Escape]</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkBox" id="screenshotHelpBox">
                        <property name="visible">True</property>
                        <property name="can_focus">False</property>
                        <property name="orientation">vertical</property>
                        <child>
                          <object class="GtkImage" id="screenshotHelpImage">
                            <property name="visible">True</property>
                            <property name="can_focus">False</property>
                            <property name="tooltip_text" translatable="yes">Screenshots

- Here one can take screenshots of the display as either a vector image (svg, ps, pdf) or a bitmap image (png).</property>
                            <property name="stock">gtk-dialog-question</property>
                            <property name="icon_size">3</property>
                          </object>
                          <packing>
                            <property name="expand">False</property>
                            <property name="fill">True</property>
                            <property name="position">0</property>
                          </packing>
                        </child>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">2</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="position">8</property>
                  </packing>
                </child>
                <child type="tab">
                  <placeholder/>
                </child>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">1</property>
              </packing>
            </child>
          </object>
          <packing>
            <property name="expand">False</property>
            <property name="fill">True</property>
            <property name="position">1</property>
          </packing>
        </child>
      </object>
    </child>
  </object>
</interface>
|]
