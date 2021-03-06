# HardCaml cycle accurate simulator using incremental

Uses incremental as the scheduling engine to implement a cycle
accurate simulator.

The hardware circuit can be thought of as follows:

```
            +---------------+     +-----------+
            |               |<--->| Registers |
            | Combinational |     +-----------+
inputs ---->|    Logic      |
            |               |     +----------+
            |               |<--->| Memories |
            |               |     +----------+
            |               |
            |               |----> outputs
            +---------------+
```

The registers and memories comprise the simulation state and are
read and written during a simulation cycle.

Given a new set of circuit inputs and the current simulation state 
the combinatorial logic is updated to generate the next simulation
state and circuit outputs.  Incremental is used to perform
the combinatorial update.

# Simulation cycle

A cycle consists of the following steps

* Set inputs and current register and memory state on the incremental
  graph.
* Stabilize the incremental graph
* Set next register and memory state on the incremental graph
* Stabilize 
* Read outputs

# Representing registers

Registers are represented in the simulator seperately to the incremental
graph.  Special inputs and outputs are created within the graph which are
used to provide the current and compute the next value of each register.

The reason this is required is that the hardware graph may have cycles,
though they may only pass through registers.  All such cycles are broken
by extracting registers and performing their updates seperately.

# Representing memories

Similarly to registers, memories may cause cycles, and thus need to be
represented by special inputs and outputs.

Memories are represented by a `(default, map)` pair.  The map stores the 
data for each address.  It is initially empty.  The default value is used 
when the map is read but doesn't yet contain a value.  A reset or clear
operation on the memory is implemented by setting the appropriate
default value and clearing the map.

*Note; to read the memory we need to index the map by the read address
of the memory.  A combinatorial loop from the memory output to the 
read address would cause a problem (I suspect stack overflow during
graph construction).  It also makes no hardware sense, but should be
better handled.*

# Cutoff

**experimental**

|type|equality|
|-|-|
| inputs            | poly |
| register state    | poly |
| memory state      | phys |
| memory read data  | poly |
| all others        | phys |

Using polymorphic equality allows portions of the graph to remain unchanged
during stabilization because the inputs, registers or memories are unchanged.

Polymorphic equality is more expensive than the default of physical equality.
Indeed, nearly as expensive and some of the operations themselves so we only
perform them around the inputs and state.

