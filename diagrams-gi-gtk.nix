{ mkDerivation, base, cairo, diagrams-cairo, diagrams-lib, fetchgit
, gi-cairo, gi-gdk, gi-gtk, lib, transformers
}:
mkDerivation {
  pname = "diagrams-gi-gtk";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/tkemps/diagrams-gi-gtk";
    sha256 = "0c4jgwmd4s9f6ipdzr6afwrxwahpr5z9azicxh25ik4f4xjn9zjy";
    rev = "ba48437dd6357c8e6624166e7131ed3b46743fcc";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base cairo diagrams-cairo diagrams-lib gi-cairo gi-gdk gi-gtk
    transformers
  ];
  homepage = "http://projects.haskell.org/diagrams/";
  description = "Backend for rendering diagrams directly to GTK windows based on gi-gtk package";
  license = lib.licenses.mit;
}
