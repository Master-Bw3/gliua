defmodule GluaTest do
  use ExUnit.Case
  doctest Glua

  test "add" do
    assert Glua.add(1, 2) == 3
  end

  test "owo" do
    assert Glua.owo(1, 2) == -1
  end
end
