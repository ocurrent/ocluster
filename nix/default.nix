{ lib, pkgs, stdenv, opam2nix, obuilder }:
let
	# TODO update to use `make`
	args = {
		inherit (pkgs.ocaml-ng.ocamlPackages_4_10) ocaml;
		selection = ./opam-selection.nix;
		src = ../.;
		override = { pkgs, selection}: {
			obuilder-spec = super: super.overrideAttrs (o: {
				src = obuilder;
			});
			obuilder = super: super.overrideAttrs (o: {
				src = obuilder;
			});
		};
	};
	opam-selection = opam2nix.build args;
	resolve = opam2nix.resolve args [
		"ocluster.opam"
		"ocluster-api.opam"
		"current_ocluster.opam"
	];

	# ocluster depends on ocluster-api, but we don't actually want to include that
	# in our shell's buildInputs
	buildInputs = lib.filter
		(drv: !(lib.hasPrefix "ocluster" drv.name || lib.hasInfix "ocluster" drv.name))
		(opam2nix.buildInputs args);
in
{
	selection = opam-selection;

	buildInputs = lib.warn (builtins.toJSON (map (d: d.name) buildInputs)) buildInputs;
	shell = opam-selection.ocluster;
	inherit (opam-selection) ocluster;
	inherit resolve;
	inherit pkgs;
}
