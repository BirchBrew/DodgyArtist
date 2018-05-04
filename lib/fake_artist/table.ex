defmodule FakeArtist.Table do
  use GenServer

  # Public API
  def start_link(default) do
    GenServer.start_link(__MODULE__, default)
  end

  def update_name_tag(pid, {id, name_tag}) do
    GenServer.call(pid, {:update_name_tag, {id, name_tag}})
  end

  # Server Callbacks
  def init([name]) do
    {:ok, {%{}, name}}
  end

  def handle_call({:update_name_tag, {id, name_tag}}, _from, {state_map, table_name}) do
    state_map = Map.put(state_map, id, name_tag)

    FakeArtistWeb.Endpoint.broadcast("table:#{table_name}", "update", %{
      names: Map.values(state_map)
    })

    {:reply, :ok, {state_map, table_name}}
  end
end
