defmodule FakeArtistWeb.RoomChannel do
  use Phoenix.Channel
  alias FakeArtistWeb.Presence

  def join("rooms:lobby", %{}, socket) do
    send(self(), {:after_join})
    {:ok, socket}
  end

  def handle_in("new:msg", %{"body" => msg}, socket) do
    user_name = socket.assigns[:user_name]

    broadcast(socket, "new:msg", %{body: msg, user: user_name})
    {:reply, :ok, socket}
  end

  def handle_in("next:player", %{}, socket) do
    users = Presence.list(socket)
    keys = users |> Map.keys()

    random_key = Enum.random(keys)

    broadcast(socket, "next:player", %{next_player: random_key})
    {:reply, :ok, socket}
  end

  def handle_info({:after_join}, socket) do
    new_player_id = Presence.list(socket) |> Map.keys() |> length()
    push(socket, "presence_state", Presence.list(socket))
    {:ok, _ref} = Presence.track(socket, new_player_id, %{online_at: now()})
    {:noreply, assign(socket, :user_name, new_player_id)}
  end

  def terminate(_reason, socket) do
    {:noreply, socket}
  end

  defp now do
    System.system_time(:seconds)
  end
end
