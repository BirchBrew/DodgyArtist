defmodule FakeArtist.Hostess do
  use GenServer
  require Logger

  @chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  @length_of_room_name 1

  # Client API
  def start_link(default) do
    GenServer.start_link(__MODULE__, default, name: __MODULE__)
  end

  def join_existing_table(table_name) do
    GenServer.call(__MODULE__, {:join_existing_table, table_name})
  end

  def start_new_table() do
    GenServer.call(__MODULE__, :start_new_table)
  end

  def get_table_pid(topic) do
    GenServer.call(__MODULE__, {:get_table_pid, topic})
  end

  def forget_table_pid(pid) do
    GenServer.call(__MODULE__, {:forget_table_pid, pid})
  end

  # Server Callbacks
  def init(_args) do
    {:ok, %{}}
  end

  def handle_call({:join_existing_table, table_name}, _from, tables) do
    case try_join_existing(table_name, tables) do
      {:ok, %{pid: pid, topic: topic}} ->
        {:reply, {:ok, pid, topic}, tables}

      {:error, message} ->
        {:reply, {:error, message}, tables}
    end
  end

  def handle_call(:start_new_table, _from, tables) do
    case try_join_new(tables) do
      {:ok, updated_tables, %{name: name, pid: pid, topic: topic}} ->
        Process.monitor(pid)
        {:reply, {:ok, %{name: name, pid: pid, topic: topic}}, updated_tables}

      {:error, message} ->
        {:reply, {:error, message}, tables}
    end
  end

  def handle_call({:get_table_pid, topic}, _from, tables) do
    {_name, {pid, _topic}} = Enum.find(tables, fn {_name, {_pid, t}} -> t == topic end)
    {:reply, pid, tables}
  end

  def handle_call({:forget_table_pid, pid}, _from, tables) do
    Logger.info(fn -> "hostess forgot about table with pid #{inspect(pid)}" end)
    tables = remove_table_pid(tables, pid)
    {:reply, :ok, tables}
  end

  def handle_info({:DOWN, _ref, :process, pid, _reason}, tables) do
    Logger.info(fn -> "hostess lost connection to table with pid #{inspect(pid)}" end)
    tables = remove_table_pid(tables, pid)
    {:noreply, tables}
  end

  # Helpers
  defp remove_table_pid(tables, pid) do
    # Maybe we should just have two maps? So we can go both ways in O(1)?
    Logger.info(fn -> "tables before removal: #{inspect(tables)}" end)
    table_name_for_pid = Enum.find(tables, fn {_, {val, _}} -> val == pid end) |> elem(0)
    tables = Map.delete(tables, table_name_for_pid)
    Logger.info(fn -> "tables after removal: #{inspect(tables)}" end)
    tables
  end

  defp try_join_existing(table_name, tables) do
    if Map.has_key?(tables, table_name) do
      {pid, topic} = Map.get(tables, table_name)
      {:ok, %{pid: pid, topic: topic}}
    else
      {:error, "The room doesn't exist"}
    end
  end

  defp try_join_new(tables) do
    with {:ok, name} <- get_unique_name(tables),
         topic = "#{System.unique_integer([:positive])}",
         {:ok, pid} <- create_process(topic),
         tables = Map.put(tables, name, {pid, topic}) do
      {:ok, tables, %{name: name, pid: pid, topic: topic}}
    else
      _ -> {:error, "Couldn't create the room"}
    end
  end

  defp generate_new_name() do
    code_points = String.codepoints(@chars)

    Enum.reduce(1..@length_of_room_name, [], fn _i, acc ->
      [Enum.random(code_points) | acc]
    end)
    |> Enum.join("")
  end

  defp get_unique_name_helper(tables, name \\ "A", tries \\ 0) do
    unless Map.has_key?(tables, name) do
      {:ok, name}
    else
      if tries > 1000 do
        {:error}
      else
        get_unique_name_helper(tables, generate_new_name(), tries + 1)
      end
    end
  end

  defp get_unique_name(tables) do
    get_unique_name_helper(tables)
  end

  defp create_process(topic) do
    {:ok, pid} = FakeArtist.DynamicSupervisor.start_child(topic)
    {:ok, pid}
  end
end
