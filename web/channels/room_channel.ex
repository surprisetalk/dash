defmodule Dash.RoomChannel do
  use Phoenix.Channel

  def join("room:lobby", _message, socket) do
    {:ok, socket}
  end

  def handle_in("queue:update", %{"version" => version, "queue" => q}, socket) do
    :mnesia.create_schema([node()])
    :mnesia.start()
    :mnesia.create_table(Queue, [type: :ordered_set])
    :mnesia.dirty_write({Queue, version, q})
    [{Queue,v_,q_}] = :mnesia.dirty_read({Queue, :mnesia.dirty_last(Queue)})

    # table = :mnesia.new(:queues, [:ordered_set, :public])
    # :mnesia.insert_new(table, {version, q})
    # [{v_,q_}] = :mnesia.lookup(table, :mnesia.last(table))

    broadcast! socket, "queue:update", %{"version" => v_, "queue" => q_}
    {:noreply, socket}
  end

  def handle_in(_, _, socket) do
    {:noreply, socket}
  end

  # intercept ["queue:update"]

  # def handle_out("queue:update", payload, socket) do
  #   push socket, "queue:update", payload
  #   {:noreply, socket}
  # end
end
