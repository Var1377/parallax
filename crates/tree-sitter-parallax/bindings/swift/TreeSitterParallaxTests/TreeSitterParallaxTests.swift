import XCTest
import SwiftTreeSitter
import TreeSitterParallax

final class TreeSitterParallaxTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_parallax())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Parallax grammar")
    }
}
