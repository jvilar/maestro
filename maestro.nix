{ mkDerivation, base, diagrams-cairo, diagrams-core
, diagrams-gi-gtk, diagrams-lib, gi-gdk, gi-gtk, gtk3, lib, mtl
, pango, SVGFonts, text
}:
mkDerivation {
  pname = "maestro";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base diagrams-cairo diagrams-core diagrams-gi-gtk diagrams-lib
    gi-gdk gi-gtk gtk3 mtl pango SVGFonts text
  ];
  license = lib.licenses.gpl2Plus;
  mainProgram = "maestro";
}
