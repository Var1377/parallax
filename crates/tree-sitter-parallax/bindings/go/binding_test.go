package tree_sitter_parallax_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_parallax "github.com/tree-sitter/tree-sitter-parallax/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_parallax.Language())
	if language == nil {
		t.Errorf("Error loading Parallax grammar")
	}
}
