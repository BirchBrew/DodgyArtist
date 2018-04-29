defmodule FakeArtistWeb.Presence do
  use Phoenix.Presence,
    otp_app: :fake_artist,
    pubsub_server: FakeArtistWeb.PubSub
end
