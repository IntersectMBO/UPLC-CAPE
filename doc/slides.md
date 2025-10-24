# UPLC-CAPE Workshop Presentation

Conference: UPLC Conference Duration: 60 minutes Format: Presentation (30-35 min) + Hands-on Workshop (25-30 min)

=== Slide 1 ====================================================================

## The Multiplication Principle

**Every validator node runs your code independently**

- 3,000+ validator nodes on Cardano
- Each node executes **every script** in **every transaction**
- Your code runs thousands of times per transaction

**The Cost Model**

- Each evaluation step: **29,773 CPU units + 100 memory units**
- A small inefficiency repeated 100 times = **2.9M extra CPU units**
- Unlike Web2: you can't "scale up servers"—**every node pays the cost**

--- Speaker Notes: -------------------------------------------------------------

**Opening hook:** "Imagine you've deployed a smart contract on Cardano. Every time someone uses it, 3,000+ validator nodes execute your code independently. A single inefficiency—repeated just 100 times in your validator—costs an extra 2.9 million CPU units. Per transaction. Multiplied across thousands of nodes."

**Key emphasis:**

- This is the fundamental mental model for UPLC performance
- Unlike traditional software where inefficiency is linear, here it's multiplicative
- The decentralized nature is a feature, but it makes optimization critical

**For mixed audience:**

- Compiler authors: "This is why your optimization passes matter at scale"
- dApp developers: "This is why you can't just 'throw more servers at it'"
- Newcomers: "Think of it as 3,000 computers all running your code simultaneously"

**Transition:** "So what happens when your script is too expensive?"

=== Slide 2 ====================================================================

## Hard Limits with Real Consequences

**Transaction Limits (Non-Negotiable)**

- Max memory: **14M units**
- Max CPU: **10B steps**
- Max script size: **16KB**

**What happens when you exceed these?**

- ❌ Transaction fails
- ❌ Collateral forfeited
- ❌ No partial execution, no refunds

**Bottom line:** Optimization isn't optional—it's required for your script to execute.

--- Speaker Notes: -------------------------------------------------------------

**Key emphasis:**

- These aren't soft warnings or best practices—they're hard protocol limits
- Collateral forfeit is the critical point: users lose money if your script is too expensive
- These limits exist for network health (prevent DoS), but they constrain what you can build

**For mixed audience:**

- Compiler authors: "Your output must fit within these bounds"
- dApp developers: "Test edge cases—what's your worst-case execution path?"

**Potential question:** "Can these limits change?"

- Answer: Yes, via governance/parameter updates, but they're currently stable. Don't count on them increasing soon.

**Transition:** "So how do these limits translate to actual costs for users?"

=== Slide 3 ====================================================================

## Concrete Cost Impact

**Fee Formula**

```
Fee = (memory × 0.0577) + (CPU_steps × 0.0000721) lovelace
```

**Real Transaction Costs**

- Simple transfers: ~**0.164 ADA**
- Script transactions: **0.5-2+ ADA**

**Optimization Impact**

- Typical optimization gains: **40-60% cost reduction**
- High-volume protocols: **$24k-$150k annual savings**

--- Speaker Notes: -------------------------------------------------------------

**Key emphasis:**

- Make the economics concrete: script transactions cost 3-12x more than simple transfers
- The formula shows memory and CPU both matter (different weights)
- For dApp developers: 40-60% reduction is realistic, not theoretical

**Additional context if time permits:**

- Minswap example: 1M transactions/year × 0.02 ADA savings = $24k annually
- Developer ROI: 1-2 weeks optimization work = 400-900% first-year return

**For mixed audience:**

- Compiler authors: "Your optimizer directly impacts these numbers"
- dApp developers: "Your users pay these fees—optimization improves UX"

**Transition:** "These aren't just theoretical numbers. Let's look at a real-world case..."

=== Slide 4 ====================================================================

## Real-World Impact—SundaeSwap

**SundaeSwap V1 → V3 Rewrite**

- V1: **164 orders** per transaction
- V3: **2,258 orders** per transaction
- **13.8x throughput improvement** from rewriting in Aiken

**What Changed?**

- Rewrote validator logic for efficiency
- Better compilation strategy
- Same functionality, dramatically different performance

**Lesson:** The language/compiler you choose matters enormously.

--- Speaker Notes: -------------------------------------------------------------

**Key emphasis:**

- This is the headline number: 13.8x is massive and memorable
- Same functionality, different implementation = order of magnitude difference
- This validates why benchmarking matters: you need to compare compilers objectively

**Additional context:**

- SundaeSwap V1 launch caused network congestion, required emergency parameter increases
- V3 solved this through better performance, not protocol changes
- This level of improvement isn't unusual—seen across multiple protocols

**For mixed audience:**

- Compiler authors: "This is your success story—show what your optimizer can do"
- dApp developers: "Choosing the right toolchain has massive implications"
- Newcomers: "Optimization isn't micro-optimization, it's transformational"

**Transition:** "So who should care about measuring UPLC performance? Let's break down the stakeholders..."

=== Slide 5 ====================================================================

## Who Benefits—Compiler Authors

**Frame of Reference for Optimization**

- Compare your compiler's optimization pipeline against others
- Validate that your optimizer is competitive

**Apples-to-Apples Comparison**

- Fixed-algorithm scenarios: `fibonacci_naive_recursion`, `factorial_naive_recursion`
- Same algorithm across all compilers → **differences come from optimization quality**
- Not about algorithm cleverness, about **compiler effectiveness**

**This is an Opportunity, Not a Competition**

- Show off what your optimizer can do
- Identify areas for improvement
- Learn from other approaches

--- Speaker Notes: -------------------------------------------------------------

**Key emphasis:**

- This is for the compiler/language authors in the room
- CAPE gives you a way to prove your optimizer is competitive
- The "apples-to-apples" concept is crucial
- **Emphasize heavily**: This is an opportunity to showcase and improve, not a competitive ranking

**Main talking points:**

- "When the algorithm is fixed, the benchmark isolates compiler quality"
- "You're not competing on who writes the cleverest algorithm—you're showing how well your optimizer works"
- "This is validation: submit your best output and see how it stacks up"
- "It's about learning and improving together"

**Example to elaborate:**

- "Imagine 5 different compilers all implement the same naive recursive fibonacci"
- "Same source logic, different UPLC output"
- "The performance difference comes purely from your optimization passes: inlining, dead code elimination, constant folding"
- "This tells you exactly how good your optimizer is"

**For compiler authors specifically:**

- Frame this positively: it's about making the whole ecosystem better
- "See what techniques others use—learn from each other"
- "Identify where you have room to improve"

**Transition:** "But compiler authors aren't the only beneficiaries. What about application developers?"

=== Slide 6 ====================================================================

## Who Benefits—dApp Developers

**Repository of Best Practices**

- Learn from compiler experts' submissions
- Study efficient validator patterns contributed by the community
- See how experts structure performant code

**Knowledge Dissemination**

- Compiler authors contribute optimized implementations
- dApp developers learn from these examples
- **Cross-pollination**: techniques from one ecosystem inspire patterns in another

**Practical Learning**

- "Why is this submission faster?" → study the approach
- Apply learned patterns to your own validators
- Improve your code without becoming a compiler expert

--- Speaker Notes: -------------------------------------------------------------

**Key emphasis:**

- CAPE isn't just rankings—it's a learning resource
- The benchmark becomes a repository of battle-tested patterns
- You don't need to build a compiler to benefit

**Main talking points:**

- "Every submission is a case study in efficient UPLC code"
- "Study the top submissions—see how experts solve common problems"
- "Learn patterns you can apply to your own validators"

**Concrete examples:**

- "How do the fastest submissions handle list operations?"
- "What patterns emerge in the top-performing recursive implementations?"
- "Can I adapt this approach to my own use case?"

**Knowledge flow:**

- Compiler authors contribute their best optimized code
- dApp developers study these implementations
- Community learns collectively—not siloed by toolchain

**For dApp developers specifically:**

- "You don't need to understand the compiler internals"
- "Focus on the patterns: how is data structured? How are operations sequenced?"
- "Use CAPE as your performance education resource"

**Transition:** "And for newcomers just entering the ecosystem..."

=== Slide 7 ====================================================================

## Who Benefits—New Developers

**Making Informed Decisions**

- "Which compiler ecosystem should I invest time in?"
- See **objective performance data** before committing resources
- Compare toolchains based on real benchmarks, not marketing claims

**Understanding Trade-offs**

- Different compilers have different strengths
- Some optimize for size, others for speed
- CAPE shows you the actual differences

**Reducing Barrier to Entry**

- Avoid costly mistakes: don't choose a slow compiler for performance-critical apps
- See the full landscape of available tools
- Make evidence-based decisions

--- Speaker Notes: -------------------------------------------------------------

**Key emphasis:**

- For newcomers, toolchain choice is a major decision
- CAPE provides objective data to inform that choice
- Reduces risk of committing to the wrong ecosystem

**Analogy to use:**

- "Choosing a compiler is like choosing a programming language"
- "You want to see real performance data before investing months of learning"
- "CAPE gives you that data upfront"

**Main talking points:**

- "Should you learn Plinth? Aiken? OpShin?"
- "CAPE shows you real performance across these ecosystems"
- "Different tools for different needs—pick the right one for your use case"

**For new developers specifically:**

- "You don't need to try every compiler"
- "Look at the scenarios closest to your planned application"
- "See which compilers perform best for your use case"

**Risk reduction:**

- "Avoid spending months learning a toolchain that's too slow for your needs"
- "See the trade-offs before you commit"
- "Evidence-based decision making"

**Potential question:** "Can I submit to CAPE if I'm not a compiler author?"

- Answer: "Absolutely! Anyone can submit. Compiler authors often contribute, but dApp developers optimizing specific use cases are equally valuable."

**Transition:** "So we've established why performance matters and who benefits. But measuring UPLC performance accurately is harder than it looks. Let's talk about the challenges..."

=== Slide 8 ====================================================================

## The Benchmarking Challenge

**Reproducibility & Determinism**

- Need **deterministic measurement** - same code, same results, every time
- Eliminate environment-specific variations

**Source Code Transparency**

- Publish sources so readers can **understand why these results**
- Clear mental model: "If I do **this** in source → I get **this** in metrics"
- Trace from source code to performance outcome

**Technical Challenges**

- **Abstract off-chain parts** for validators with multi-transaction interactions
- One-shot algorithms vs complex validator logic

**Usability Challenges**

- Present data that's **easy to compare and digest**
- **Lower barrier to contribution** - enable crowdsourcing
- Make it accessible for non-experts to participate

--- Speaker Notes: -------------------------------------------------------------

**Key emphasis:**

- These are real problems that have prevented good benchmarking in the past
- CAPE was designed to solve each of these challenges
- This sets up the "Introducing CAPE" slide

**Break down each challenge:**

**1. Reproducibility & Determinism:**

- "Before CAPE: 'It runs fast on my machine' isn't useful"
- "Need exact same results regardless of who runs it, when, or where"
- "Critical for trust and validation"

**2. Source Code Transparency:**

- "Black box benchmarks don't teach anyone anything"
- "You need to see: here's the source, here's how it compiles, here's why it performs this way"
- "The mental model matters: developers need to connect source patterns to performance outcomes"

**3. Technical Challenges:**

- "Many validators aren't one-shot algorithms - they have state, multi-tx interactions"
- "Need to abstract the off-chain setup/interaction parts"
- "Focus benchmark on the validator logic itself"

**4. Usability Challenges:**

- "Data needs to be comparable - apples to apples"
- "Present results in digestible format - not raw dumps"
- "Crowdsourcing: anyone should be able to contribute without being a benchmarking expert"

**Transition:** "So how does CAPE address these challenges? Let me introduce the framework..."

=== Slide 9 ====================================================================

## Introducing UPLC-CAPE

**CAPE = Cardano Application Performance Evaluation**

**Developed & maintained by the Plutus Core team**

- Unbiased, **fair field of play** for all compilers
- No favoritism, just objective measurement

**Focus on Writing Code, Not Measurement**

- `cape` command-line tool handles all the complexity
- Authors focus on: write code → compile to UPLC → submit
- Measurement, validation, aggregation handled automatically

**Same Environment as Mainnet**

- Uses **sandboxed CEK machine** (UPLC interpreter)
- Same implementation used by Cardano nodes on mainnet
- Uses **latest cost model** deployed on mainnet - authors don't need to think about it

**What's Included**

- **Scenarios**: Benchmark problems (growing set, open for contributions)
- **Tooling**: `cape` CLI for easy contribution workflow
- **Visualization**: Tables and graphs on the web for easy comparison

**Open & Community-Driven**

- Anyone can contribute scenarios or submissions
- Transparent, reproducible, collaborative

--- Speaker Notes: -------------------------------------------------------------

**Important terminology note:**

- When we say "performance" in CAPE, we mean **effectiveness** broadly
- NOT wall clock time - we're measuring cost model units (CPU/memory), script size
- "Performance" = how efficient your UPLC code is within Cardano's execution model

**Key emphasis:**

- Spell out the acronym first - make it clear what CAPE stands for
- Emphasize the Plutus Core team backing - credibility and neutrality
- The "fair field of play" point is important - no one has an advantage
- Make it clear how much CAPE simplifies contribution
- Mainnet equivalence is crucial - results are real-world relevant

**Main talking points:**

**CAPE acronym:**

- "CAPE stands for Cardano Application Performance Evaluation"
- "It's the official benchmarking framework for UPLC performance"

**Plutus Core team & neutrality:**

- "Developed and maintained by the Plutus Core team at IOG"
- "This isn't a vendor benchmark - it's unbiased infrastructure"
- "Fair field of play: everyone uses the same measurement methodology"

**Focus on writing code:**

- "Here's what CAPE does for you: you don't need to become a benchmarking expert"
- "Write your code, compile it to UPLC, submit it with the `cape` command"
- "CAPE handles: measurement, validation against test cases, metric aggregation"
- "This lowers the barrier - you can focus on what you're good at"

**Mainnet equivalence:**

- "CAPE uses the exact same CEK machine implementation that runs on Cardano mainnet"
- "It's sandboxed for safety, but it's the same interpreter your code will run on in production"
- "Uses the latest cost model from mainnet - you don't need to track cost model updates"
- "This means: CAPE results are real-world relevant, not theoretical"

**What's included:**

- "Scenarios: benchmark problems like fibonacci, factorial, list operations"
- "The set isn't huge yet, but it's growing - and open for contributions"
- "Tooling: the `cape` CLI guides you through the contribution process"
- "Visualization: results published on the web with tables and graphs for easy comparison"

**Open & community-driven:**

- "This is collaborative, not competitive"
- "Anyone can contribute new scenarios or submissions"
- "Everything is transparent and reproducible"

**Transition:** "Let me show you the core entities in CAPE..."

=== Slide 10 ===================================================================

## CAPE Architecture: Core Entities

```
┌─────────────────────────────────────┐
│  Scenario (aka Benchmark)          │
│                                     │
│  - Problem definition               │
│  - Test suite (cape-tests.json)    │
│  - Expected behavior                │
│                                     │
│  Examples:                          │
│  • fibonacci                        │
│  • fibonacci_naive_recursion        │
│  • factorial_naive_recursion        │
└─────────────────┬───────────────────┘
                  │
                  │ has many
                  ↓
┌─────────────────────────────────────┐
│  Submission                         │
│                                     │
│  - UPLC implementation              │
│  - Source code                      │
│  - Metadata (compiler, version)     │
│  - Metrics (CPU, memory, size)      │
│                                     │
│  Format:                            │
│  {Compiler}_{version}_{contributor} │
│                                     │
│  Example:                           │
│  Plinth_1.11.0_yura                 │
└─────────────────────────────────────┘
```

**Key Relationship**

- One scenario → many submissions
- Different compilers, different authors, same problem
- All submissions validated against the same test suite

--- Speaker Notes: -------------------------------------------------------------

**Key emphasis:**

- This is the fundamental structure of CAPE
- Scenarios define "what to solve", submissions define "how to solve it"
- The one-to-many relationship enables comparison

**Diagram explanation:**

**Scenario (Benchmark):**

- "A scenario is a problem definition"
- "Contains: problem spec, test suite (cape-tests.json), expected behavior"
- "Examples: fibonacci, fibonacci_naive_recursion, factorial_naive_recursion"
- "Note: we use 'scenario' and 'benchmark' interchangeably"

**Submission:**

- "A submission is one implementation of a scenario"
- "Contains: compiled UPLC, source code, metadata, measured metrics"
- "Naming: {Compiler}_{version}_{contributor}"
- "Example: Plinth_1.11.0_yura means Plinth compiler version 1.11.0 by contributor yura"

**The Relationship:**

- "One scenario has many submissions"
- "Different compilers solving the same problem"
- "All validated against the same test suite - apples to apples"
- "This structure enables fair comparison"

**Transition:** "Now let me show you how these entities work together in practice..."

=== Slide 11 ===================================================================

## How CAPE Works

**The Workflow**

1. **Scenario Creation**: Document problem, create test suite (`cape-tests.json`)
2. **Submissions**: Authors submit implementations with sources
3. **Two Types of Scenarios**:
   - **Fixed algorithm** (`*_naive_recursion`): Test compiler optimizations only
   - **Flexible algorithm** (`fibonacci`): Authors compete, collectively discover optimal approach
4. **Measurement**: `cape` runs UPLC on CEK machine, collects metrics
5. **Reporting**: Generate HTML reports with tables and graphs
6. **GitHub Actions**: Automatic preview reports for PRs

**Key Commands**

```bash
cape benchmark list              # See available scenarios
cape benchmark new <name>        # Create new scenario from template
cape submission new <scenario>   # Create submission structure
cape submission measure <path>   # Collect metrics
cape submission verify <path>    # Validate correctness
cape submission report           # Generate HTML report
```

--- Speaker Notes: -------------------------------------------------------------

**Key emphasis:**

- The workflow creates a virtuous cycle: scenarios → submissions → discovery
- Distinguish between fixed-algorithm (compiler testing) vs flexible-algorithm (collective optimization)
- Automation is key: authors focus on code, tools handle the rest

**Workflow explanation:**

**1. Scenario Creation:**

- "Someone identifies a problem worth benchmarking"
- "Document it in a scenario spec, write cape-tests.json with test cases"
- "This defines the problem everyone will solve"

**2. Submissions:**

- "Authors submit their implementations: UPLC code + sources"
- "Sources are important - transparency, learning, reproducibility"

**3. Two Types of Scenarios:**

- "Fixed algorithm scenarios have `_naive_recursion` suffix"
- "These lock the algorithm - everyone implements the same approach"
- "Performance differences come purely from compiler optimization quality"
- "Example: fibonacci_naive_recursion, factorial_naive_recursion"
-
- "Flexible algorithm scenarios allow any approach"
- "Authors compete, try different strategies"
- "Over time, submissions converge on the optimal implementation"
- "Example: fibonacci - we expect the community to discover the best approach collectively"

**4. Measurement:**

- "cape runs your UPLC on the CEK machine - same as mainnet"
- "Collects CPU units, memory units, script size"
- "All deterministic, all reproducible"

**5. Reporting:**

- "Results published as HTML with tables and graphs"
- "Easy visual comparison across submissions"

**6. GitHub Actions:**

- "Submit a PR → automatic preview report generated"
- "See how your submission compares before merging"

**Key commands walkthrough:**

- "cape benchmark list - see what scenarios are available"
- "cape benchmark new - create a new scenario from template"
- "cape submission new - creates the submission structure for you"
- "cape submission measure - runs tests, generates metrics.json"
- "cape submission verify - checks everything is valid"
- "cape submission report - generates HTML visualization"

**Visual note:**

- "I may create a diagram showing this flow for the actual slides"

**Transition:** "Now that you understand the workflow, let's talk about what we're actually measuring..."

=== Slide 12 ===================================================================

## Key Metrics Explained

**Two Categories of Metrics**

1. **Raw Metrics** - Direct measurements from UPLC evaluation
   - CPU units, Memory units
   - Script size (bytes), Term size (AST nodes)
2. **Derived Metrics** - Calculated from raw metrics, connected to real-world use-cases
   - Transaction fees (lovelace/ADA)
   - Budget utilization (% of tx/block limits)
   - Capacity (scripts per tx/block)

**Aggregation Strategies**

For scenarios with multiple test cases:

- `maximum` - Worst-case across all tests (used for budget/capacity metrics)
- `sum` - Total across all tests (used for fee metrics)
- `minimum`, `median` - Best-case and middle values
- `sum_positive`, `sum_negative` - Success vs failure cases

**Trade-offs & Variants**

- Optimizations involve trade-offs: size vs speed, memory vs CPU
- **Variants** allow submissions to indicate which side of a trade-off is optimized for
- Example: "size-optimized" vs "speed-optimized" variants

--- Speaker Notes: -------------------------------------------------------------

**Key emphasis:**

- Distinguish between raw (what we measure) and derived (what users care about)
- Aggregation strategies have semantic meaning - not arbitrary choices
- Trade-offs are real, variants capture different optimization goals

**Two categories explanation:**

**Raw Metrics:**

- "These are direct outputs from running UPLC on the CEK machine"
- "CPU units: computational cost"
- "Memory units: memory cost"
- "Script size: on-chain storage size in bytes"
- "Term size: AST node count, complexity indicator"

**Derived Metrics:**

- "These translate raw measurements into real-world impact"
- "Execution fee: 'How much does it cost to run this?'"
- "Budget utilization: 'What percentage of transaction/block limits does this consume?'"
- "Capacity: 'How many of these can fit in a transaction or block?'"
- "These are calculated using Conway mainnet protocol parameters"

**Aggregation strategies:**

- "`maximum` - Worst-case semantics"
- "Used for budget/capacity metrics because validators face adversarial inputs"
- "You need to handle the worst case, not just average case"
- "A validator cheap on average but expensive on edge cases = security risk"

- "`sum` - Total cost semantics"
- "Used for fee metrics to represent total test suite cost"
- "Useful for estimating verification overhead"

- "Other aggregations (`minimum`, `median`, `sum_positive`, `sum_negative`) provide additional perspectives"
- "All raw metric aggregations are published for transparency"

**Hybrid aggregation strategy:**

- "CAPE uses a hybrid approach: different aggregations for different derived metrics"
- "Budget/capacity use `maximum` (worst-case)"
- "Fees use `sum` (total cost)"
- "This makes gaming harder - you can't optimize one without improving the other"

**Trade-offs & variants:**

- "Real optimizations involve trade-offs"
- "Example: You can make a script smaller, but it might run slower"
- "Or optimize for speed but increase memory usage"
- "Variants allow you to submit different versions optimized for different goals"
- "Example: fibonacci-size-optimized vs fibonacci-speed-optimized"
- "This is explicit - not hidden in one 'best' submission"

**Real-world example:**

- "A 10KB script with 500M CPU steps vs"
- "A 50KB script with 300M CPU steps"
- "Which is 'better'? Depends on your use case:"
- " - High-volume protocol → prefer lower execution cost"
- " - One-time deployment → script size might not matter"
- " - Reference script heavy usage → smaller size saves on reference fees"

**Transition:** "Let's look at what scenarios are currently available in CAPE..."
