defmodule FakeArtistWeb.PageController do
  use FakeArtistWeb, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
