defmodule Gliua.MixProject do
  use Mix.Project

  @app :gliua
  @version "0.1.0"

  def project do
    [
      app: @app,
      version: @version,
      elixir: "~> 1.16",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      archives: [mix_gleam: "~> 0.6"],
      compilers: [:gleam | Mix.compilers()],
      aliases: [
        "deps.get": ["deps.get", "gleam.deps.get"]

      ],
      erlc_paths: [
        "build/dev/erlang/#{@app}/_gleam_artefacts",
        "build/dev/erlang/#{@app}/build"
      ],
      erlc_include_path: "build/dev/erlang/#{@app}/include",
      prune_code_paths: false,

    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:rustler, "~> 0.32.1", runtime: false},
      {:gleam_stdlib, "~> 0.34 or ~> 1.0"},
      {:gleeunit, "~> 1.0", only: [:dev, :test], runtime: false},
    ]
  end
end
