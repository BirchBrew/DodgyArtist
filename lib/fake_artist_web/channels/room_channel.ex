defmodule FakeArtistWeb.RoomChannel do
  use Phoenix.Channel
  alias FakeArtistWeb.Presence

  def join("rooms:lobby", %{"user_id" => user_name}, socket) do
    send(self(), {:after_join, user_name})
    {:ok, socket}
  end

  def handle_in("new:msg", %{"body" => msg}, socket) do
    user_name = socket.assigns[:user_name]

    broadcast(socket, "new:msg", %{body: msg, user: user_name})
    {:reply, :ok, socket}
  end

  def handle_in("new:player", %{}, socket) do
    user_name = socket.assigns[:user_name]
    users = Presence.list(socket)
    keys = users |> Map.keys()
    random_keys_index = :rand.uniform(length(keys)) - 1
    random_key = Enum.at(keys, random_keys_index)
    broadcast(socket, "new:player", %{body: random_key, user: user_name})
    {:reply, :ok, socket}
  end

  def handle_info({:after_join, user_name}, socket) do
    push(socket, "presence_state", Presence.list(socket))
    {:ok, _ref} = Presence.track(socket, user_name, %{online_at: now()})
    {:noreply, assign(socket, :user_name, user_name)}
  end

  def terminate(_reason, socket) do
    {:noreply, socket}
  end

  defp now do
    System.system_time(:seconds)
  end
end
