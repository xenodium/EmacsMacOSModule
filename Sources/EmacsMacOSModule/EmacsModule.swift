// Copyright (C) 2023 Alvaro Ramirez
//
// Author: Alvaro Ramirez https://xenodium.com
// URL: https://github.com/xenodium/EmacsMacOSModule
//
// This package is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This package is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

import AppKit
import EmacsSwiftModule

class EmacsModule: Module {
  let isGPLCompatible = true

  var env: Environment?

  func Init(_ env: Environment) throws {
    try env.defun(
      "macos-module--reveal-in-finder",
      with: """
        Reveal (and select) files in ARG1 in macOS Finder.

        ARG1 mus be a vector (not a list) of file paths.
        """
    ) { (env: Environment, files: [String]) in
      NSWorkspace.shared.activateFileViewerSelecting(files.map { URL(fileURLWithPath: $0) })
    }

    try env.defun(
      "macos-module--share",
      with: """
        Share files in ARG1.

        ARG1 must be a vector (not a list) of file paths.
        """
    ) { (env: Environment, files: [String]) in
      let urls = files.map { URL(fileURLWithPath: $0) }

      let picker = NSSharingServicePicker(items: urls)
      guard let view = NSApp.mainWindow?.contentView else {
        return
      }

      let x = try env.funcall("macos--emacs-point-x") as Int
      let y = try env.funcall("macos--emacs-point-y") as Int

      let rect = NSRect(
        x: x + 15, y: Int(view.bounds.height) - y + 15, width: 1, height: 1
      )
      picker.show(relativeTo: rect, of: view, preferredEdge: .maxY)
    }

    try env.defun(
      "macos-module--sharing-services",
      with: """
        Return sharing services available for files in ARG1.

        ARG1 must be a vector (not a list) of file paths.
        """
    ) { (env: Environment, files: [String]) in
      return NSSharingService.sharingServices(forItems: [""]).map { $0.title }
    }

    try env.defun(
      "macos-module--show-emoji-picker",
      with: "Show emoji picker (macOS module implementation)."
    ) { (env: Environment) in
      NSApp.orderFrontCharacterPalette(nil)
    }
    try env.defun(
      "macos-module--quick-look",
      with: """
        Preview files in ARG1 with macOS Quick Look, selecting ARG2.

        ARG1 must be a vector (not a list) of file paths.
        ARG2 is the zero-based index of the file to select.
        ARG3 is the vector [X Y WIDTH HEIGHT] to zoom the panel out of, in
        Emacs display coordinates, or an empty vector to fade in instead.

        Emacs keeps keyboard focus, so calling this repeatedly swaps the
        previewed file in place.
        """
    ) { (env: Environment, files: [String], index: Int, sourceRect: [Int]) in
      let urls =
        files
        .filter { FileManager.default.fileExists(atPath: $0) }
        .map { URL(fileURLWithPath: $0) }
      QuickLookPreviewer.shared.show(urls, at: index, sourceRect: sourceRect)
    }

    try env.defun(
      "macos-module--quick-look-hide",
      with: "Hide the macOS Quick Look panel."
    ) { (env: Environment) in
      QuickLookPreviewer.shared.hide()
    }

    try env.defun(
      "macos-module--quick-look-visible-p",
      with: "Return non-nil if the macOS Quick Look panel is visible."
    ) { (env: Environment) in
      QuickLookPreviewer.shared.isVisible
    }
  }
}

func createModule() -> Module { EmacsModule() }
