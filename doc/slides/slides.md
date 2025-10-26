---
theme: default
title: UPLC-CAPE Workshop
info: |
  #UPLC-CAPE Workshop

  Comparative Artifact Performance Evaluation for UPLC

  Conference: UPLC Conference
  Duration: 60 minutes
  Format: Presentation (30-35 min) + Hands-on Workshop (25-30 min)
class: text-center
highlighter: shiki
drawings:
  persist: false
transition: slide-left
mdc: true
---

<div class="cover-logos">
  <img src="/uplc-logo.png" alt="UPLC Conference Logo" />
</div>

<div class="cover-content">
  <h1 class="cover-main">UPLC CAPE:</h1>
  <h1 class="cover-sub">Comparative Artifact</h1>
  <h1 class="cover-sub">Performance Evaluation</h1>

  <p class="cover-date">October 2025</p>
</div>

<!--
**Conference**: UPLC Conference
**Duration**: 60 minutes
**Format**: Presentation (30-35 min) + Hands-on Workshop (25-30 min)
-->

---
layout: center
---

## The Multiplication Principle

**Every validator node runs your code independently**

- 3,000+ validator nodes on Cardano
- Each node executes **every script** in **every transaction**
- Your code runs thousands of times per transaction

<!--
**Opening hook:** "Imagine you've deployed a smart contract on Cardano. Every time someone uses it, 3,000+ validator nodes execute your code independently."

**Key emphasis:**

- This is the fundamental mental model for UPLC performance
- The decentralized nature is a feature, but it makes optimization critical

**Why this matters:**

In a decentralized network, you can't solve performance problems by adding more servers—every optimization matters because your code runs thousands of times per transaction.

**Transition:** "Now let's look at what this means for costs..."
-->

---
layout: center
---

## The Cost is Multiplied

**The Cost Model**

- Each evaluation step: **29,773 CPU units + 100 memory units**
- A small inefficiency repeated 100 times = **2.9M extra CPU units**
- Unlike Web2: you can't "scale up servers" — **every node pays the cost**

<!--
**The cost impact:** "A single inefficiency—repeated just 100 times in your validator—costs an extra 2.9 million CPU units. Per transaction. Multiplied across thousands of nodes."

**Key emphasis:**

- Unlike traditional software where inefficiency is linear, here it's multiplicative
- Every node pays the cost—you can't scale up servers

**Transition:** "Let's make this concrete with the actual fee formula..."
-->

---
layout: center
---

## Fee Formula

```
Fee = (memory × 0.0577) + (CPU_steps × 0.0000721) lovelace
```

<!--
**Key emphasis:**

- The formula shows memory and CPU both matter (different weights)
- Both resources contribute to the final transaction cost

**Transition:** "Now let's see what these fees look like in practice..."
-->

---
layout: center
---

## Real Transaction Costs

- Simple transfers: ~**0.164 ADA**
- Script transactions: **0.5-2+ ADA**

<!--
**Key emphasis:**

- Make the economics concrete: script transactions cost 3-12x more than simple transfers
- This is the actual cost users pay today

**Transition:** "So what impact can optimization have on these costs?"
-->

---
layout: center
---

## Optimization Impact

- Typical optimization gains: **40-60% cost reduction**
- High-volume protocols: **$24k-$150k annual savings**

<!--
**Key emphasis:**

- For dApp developers: 40-60% reduction is realistic, not theoretical
- The economics are compelling

**Additional context if time permits:**

- Minswap example: 1M transactions/year × 0.02 ADA savings = $24k annually
- Developer ROI: 1-2 weeks optimization work = 400-900% first-year return

**Why this matters:**

Better optimization means lower fees for users and better user experience overall.

**Transition:** "But there's a hard ceiling on how much you can execute..."
-->

---
layout: center
---

## Hard Limits with Real Consequences

**Transaction Limits (Non-Negotiable)**

- Max memory: **14M units**
- Max CPU: **10B steps**
- Max script size: **16KB**

<!--
**Key emphasis:**

- These aren't soft warnings or best practices—they're hard protocol limits
- These limits exist for network health (prevent DoS), but they constrain what you can build

**Practical guidance:**

All compiled output must fit within these bounds—test your worst-case execution paths to ensure you stay under the limits.

**Potential question:** "Can these limits change?"

- Answer: Yes, via governance/parameter updates, but they're currently stable. Don't count on them increasing soon.

**Transition:** "So what happens when you exceed these limits?"
-->

---
layout: center
---

## Hard Limits with Real Consequences

**What happens when you exceed these?**

<div v-click>

- ❌ Transaction fails
- ❌ Collateral forfeited
- ❌ No partial execution, no refunds

</div>

<!--
**Key emphasis:**

- Collateral forfeit is the critical point: users lose money if your script is too expensive
- No second chances, no partial execution, no refunds
- The consequences are immediate and harsh

**Why this matters:**

Unlike web applications that can crash gracefully, blockchain execution is all-or-nothing—scripts either pass all checks or fail completely, forfeiting user collateral.

**Bottom line:**

- Optimization isn't optional—it's required for execution
- This is the "stick" that makes performance critical

**Transition:** "Let's see a real-world example of optimization impact..."
-->

---
layout: center
---

## Real-World Impact: SundaeSwap

<v-switch :unmount="true">
<template #0>

**SundaeSwap V1 → V3 Rewrite**

- V1: **164 orders** per transaction
- V3: **2,258 orders** per transaction
- **13.8x throughput improvement** from rewriting in Aiken

</template>
<template #1>

**What Changed?**

- Rewrote validator logic for efficiency
- Better compilation strategy
- Same functionality, dramatically different performance

</template>
</v-switch>

<!--
**Key emphasis:**

- This is the headline number: 13.8x is massive and memorable
- Same functionality, different implementation = order of magnitude difference
- This validates why benchmarking matters: you need to compare compilers objectively

**Additional context:**

- SundaeSwap V1 launch caused network congestion, required emergency parameter increases
- V3 solved this through better performance, not protocol changes
- This level of improvement isn't unusual—seen across multiple protocols

**Key takeaway:**

Optimization isn't micro-optimization—choosing the right toolchain and optimization strategy can deliver transformational performance improvements.

**Transition:** "This proves optimization matters. Now let's drive the point home..."
-->

---
layout: center
class: attention
---

## Optimization isn't optional

<!--
**Delivery:**

- Let this statement sink in
- Brief pause for emphasis
- This is a key takeaway for all audiences

**Transition:** "And equally important..."
-->

---
layout: center
class: attention
---

## The language/compiler you choose matters

<!--
**Key emphasis:**

- The SundaeSwap example showed: same functionality, different implementation = 13.8x difference
- Toolchain choice has massive implications

**Key takeaway:**

The toolchain you choose has massive implications—optimization can deliver transformational improvements, not just incremental gains.

**Transition:** "So who should care about measuring UPLC performance? Let's break down the stakeholders..."
-->

---
layout: center
---

## Benchmarking Challenges

<v-switch :unmount="true">
<template #0>

**Reproducibility & Determinism**

- Need **deterministic measurement** - same code, same results, every time
- Eliminate environment-specific variations

</template>
<template #1>

**Source Code Transparency**

- Publish sources so readers can **understand why these results**
- Clear mental model: "If I do **this** in source → I get **this** in metrics"
- Trace from source code to performance outcome

</template>
<template #2>

**Technical Challenges**

- **Abstract off-chain parts** for validators with multi-transaction interactions
- One-shot algorithms vs complex validator logic
- State management

</template>
<template #3>

**Usability Challenges**

- Present data that's **easy to compare and digest**
- **Lower barrier to contribution** - enable crowdsourcing
- Make it accessible for non-experts to participate

</template>
</v-switch>

<!--
**Key emphasis:**

- These are real problems that have prevented good benchmarking in the past
- CAPE was designed to solve each of these challenges

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
-->

---
layout: center
---

## Introducing UPLC-CAPE

<v-switch :unmount="true">
<template #0>

**CAPE = Cardano Application Performance Evaluation**

**CAPE = Comparative Artifact Performance Evaluation**

</template>
<template #1>

**Developed & maintained by the Plutus Core team**

- Unbiased, **fair field of play** for all compilers
- No favoritism, just objective measurement

</template>
<template #2>

**Focus on Writing Code, Not Measurement**

- `cape` command-line tool handles all the complexity
- Authors focus on: write code → compile to UPLC → submit
- Measurement, validation, aggregation handled automatically

</template>
<template #3>

**Same Environment as Mainnet**

- Uses **sandboxed CEK machine** (UPLC interpreter)
- Same implementation used by the last release of the Cardano node
- Uses **latest cost model** deployed

</template>
<template #4>

**What's Included**

- **Scenarios**: Benchmark problems (growing set, open for contributions)
- **Tooling**: `cape` CLI for easy contribution workflow
- **Visualization**: Tables and graphs on the web for easy comparison

</template>
<template #5>

**Open & Community-Driven**

- Anyone can contribute scenarios or submissions
- Transparent, reproducible, collaborative

</template>
</v-switch>

<!--
**Important terminology note:**

- When we say "performance" in CAPE, we mean **effectiveness** broadly
- NOT wall clock time - we're measuring cost model units (CPU/memory), script size
- "Performance" = how efficient your UPLC code is within Cardano's execution model

**Key emphasis:**

- Spell out the acronym first - make it clear what CAPE stands for
- Emphasize the Plutus Core team backing - credibility and neutrality
- The "fair field of play" point is important - no one has an advantage

**CAPE acronym:**

- "CAPE stands for Cardano Application Performance Evaluation"
- "It's the official benchmarking framework for UPLC performance"

**Plutus Core team & neutrality:**

- "Developed and maintained by the Plutus Core team at IOG"
- "This isn't a vendor benchmark - it's unbiased infrastructure"
- "Fair field of play: everyone uses the same measurement methodology"

**Key emphasis:**

- Make it clear how much CAPE simplifies contribution
- Mainnet equivalence is crucial - results are real-world relevant

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
-->

---
layout: center
---

## CAPE Architecture: Core Entities

```mermaid
%%{init: {'theme':'base', 'themeVariables': { 'primaryColor':'#058DC7', 'primaryTextColor':'#fff', 'primaryBorderColor':'#333', 'lineColor':'#333', 'secondaryColor':'#50B432', 'tertiaryColor':'#ED561B', 'fontSize':'16px'}}}%%
graph LR
    Scenario["<div style='text-align: left; padding: 10px;'><b style='font-size: 32px;'>Scenario</b><br/><br/>•&nbsp;Problem definition<br/>•&nbsp;Test suite<br/>•&nbsp;Expected behavior<br/><br/>Example:<br/>•&nbsp;fibonacci_naive_recursion</div>"]

    Submission["<div style='text-align: left; padding: 10px;'><b style='font-size: 32px;'>Submission</b><br/><br/>•&nbsp;UPLC implementation<br/>•&nbsp;Source code<br/>•&nbsp;Metadata<br/><br/>Example:<br/>•&nbsp;Plinth_1.55.0_Yura_exbudget</div>"]

    Scenario -->|"&nbsp;has many&nbsp;"| Submission

    style Scenario fill:#058DC7,stroke:#333,stroke-width:3px,color:#fff
    style Submission fill:#50B432,stroke:#333,stroke-width:3px,color:#fff
```

<!--
**Key emphasis:**

- This is the fundamental structure of CAPE
- Scenarios define "what to solve", submissions define "how to solve it"
- The one-to-many relationship enables comparison

**Scenario:**

- A scenario is a problem definition
- Contains: problem spec, test suite (cape-tests.json), expected behavior
- Examples: fibonacci, fibonacci_naive_recursion, factorial_naive_recursion
- Note: we use 'scenario' and 'benchmark' interchangeably

**Submission:**

- A submission is one implementation of a scenario uniquely identified by compiler name, version, contributor handle.
- Contains: compiled UPLC, source code, metadata, measured metrics
- Naming: {Compiler}_{version}_{contributor}
- Example: Plinth_1.11.0_yura means Plinth compiler version 1.11.0 by contributor yura

**The Relationship:**

- One scenario has many submissions
- Different compilers solving the same problem
- All validated against the same test suite

**Transition:** "Now let me show you how these entities work together in practice..."
-->

---
layout: center
---

## Who Benefits: Compiler Authors

<v-switch :unmount="true">
<template #0>

**Frame of Reference for Optimization**

- Compare your compiler's optimization pipeline against others
- Validate that your optimizer is competitive

</template>
<template #1>

**Apples-to-Apples Comparison**

- Same algorithm across all compilers → **differences come from optimization quality**
- Not about algorithm cleverness, about **compiler effectiveness**

</template>
<template #2>

**This is more of an Opportunity, less of a Competition**

- Show off what your optimizer can do
- Identify areas for improvement
- Learn from other approaches

</template>
</v-switch>

<!--
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
-->

---
layout: center
---

## Who Benefits: dApp Developers

<v-switch :unmount="true">
<template #0>

**Repository of Best Practices**

- Learn from compiler experts' submissions
- Study efficient validator patterns contributed by the community
- See how experts structure performant code

</template>
<template #1>

**Knowledge Dissemination**

- Compiler authors contribute optimized implementations
- dApp developers learn from these examples
- **Cross-pollination**: techniques from one ecosystem inspire patterns in another

</template>
<template #2>

**Practical Learning**

- "Why is this submission faster?" → study the approach
- Apply learned patterns to your own validators
- Improve your code without becoming a compiler expert

</template>
</v-switch>

<!--
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
-->

---
layout: center
---

## Who Benefits: New Developers

<v-switch :unmount="true">
<template #0>

**Making Informed Decisions**

- "Which compiler ecosystem should I invest time in?"
- See **objective performance data** before committing resources
- Compare toolchains based on real benchmarks, not marketing claims

</template>
<template #1>

**Understanding Trade-offs**

- Different compilers have different strengths
- Some optimize for size, others for speed
- CAPE shows you the actual differences

</template>
<template #2>

**Reducing Barrier to Entry**

- Avoid costly mistakes: choose the most optimal compiler for performance-critical apps
- See the full landscape of available tools
- Make evidence-based decisions

</template>
</v-switch>

<!--
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
-->

---
layout: center
---

## CAPE Workflow

<v-switch :unmount="true">
<template #0>

```mermaid
%%{init: {'theme':'base', 'themeVariables': { 'primaryColor':'#058DC7', 'primaryTextColor':'#fff', 'primaryBorderColor':'#333', 'lineColor':'#333', 'fontSize':'18px'}}}%%
graph LR
    A[Scenario] --> B[Submission]
    B --> C[Measure]
    C --> D[Report]

    style A fill:#058DC7,stroke:#333,stroke-width:3px,color:#fff
    style B fill:#50B432,stroke:#333,stroke-width:3px,color:#fff
    style C fill:#ED561B,stroke:#333,stroke-width:3px,color:#fff
    style D fill:#DDDF00,stroke:#333,stroke-width:3px,color:#333
```

</template>
<template #1>

**Create a Scenario**

1. Choose type: **Fixed algorithm** or **Flexible**
1. Create scenario from a template with `cape benchmark new <name>`
1. Document the benchmark scenario in `<scenario>.md`
1. Create test suite in `cape-tests.json`

</template>
<template #2>

**Make a Submission**

1. Create project with sources (GitHub repo or local)
1. Compile your program to UPLC (the `<scenario>.uplc` file)
1. Create submission directory from a template with  
   `cape submission new <scenario>`
1. Fill in metadata, README

</template>
<template #3>

**Measure**

`cape` runs UPLC on CEK machine, collects metrics

1. Validate correctness `cape submission verify <path>`
1. Collect metrics `cape submission measure <path>`

</template>
<template #4>

**Preview results: Web Report**

CAPE generates report with tables and graphs

- Manual preview with `cape submission report`
- Automatic preview for PRs
- Published: https://intersectmbo.github.io/UPLC-CAPE/

</template>
</v-switch>

<!--
**Key points:**

- Fixed algorithm scenarios (`_naive_recursion`) test compiler optimization quality
- Flexible scenarios allow any approach - submissions converge on optimal implementation
- Authors focus on writing code; `cape` handles measurement, validation, and reporting
- All measurements deterministic and reproducible (same CEK machine as mainnet)

**Workflow summary:**

Create scenario → Write implementation → Submit → Automated measurement → HTML reports with comparison tables

**Transition:** "Now let's talk about what metrics we're actually measuring..."
-->

---
layout: center
---

## Key Metrics Explained

<v-switch :unmount="true">
<template #0>

1. **Raw Metrics**<br> Direct measurements from UPLC evaluation
2. **Derived Metrics**<br> Calculated from raw metrics, connected to real-world use-cases

</template>

<template #1>

**Raw Metrics** — &nbsp;Direct measurements from UPLC evaluation

- Script size:
  - CBOR/Flat encoding measured in bytes
  - Term size measured in AST nodes
- Execution budget:
  - CPU units
  - Memory units

</template>

<template #2>

**Derived Metrics** — &nbsp;are meaningful for dApp developers

- Transaction fees (lovelace/ADA)
- Budget utilization (% of tx/block limits)
- Capacity (scripts per tx/block)

</template>

<template #3>

```mermaid
%%{init: {'theme':'base', 'themeVariables': { 'primaryColor':'#058DC7', 'primaryTextColor':'#fff', 'primaryBorderColor':'#333', 'lineColor':'#333', 'secondaryColor':'#50B432', 'fontSize':'18px'}}}%%
graph LR
    Program["<div style='text-align: left; padding: 10px;'><b style='font-size: 28px;'>UPLC Program</b><br/><br/>•&nbsp;Source code<br/>•&nbsp;Compiled artifact<br/>•&nbsp;<strong>Size Metrics</strong></div>"]

    Evaluation["<div style='text-align: left; padding: 10px;'><b style='font-size: 28px;'>Evaluation</b><br/><br/>•&nbsp;Test case input<br/>•&nbsp;Execution trace<br/>•&nbsp;<strong>Execution&nbsp;Budget&nbsp;Metrics</strong></div>"]

    Program -->|"&nbsp;has many&nbsp;"| Evaluation

    style Program fill:#058DC7,stroke:#333,stroke-width:3px,color:#fff
    style Evaluation fill:#50B432,stroke:#333,stroke-width:3px,color:#fff
```

</template>

<template #4>

**Aggregation Strategies**

For scenarios with multiple evaluations:

- `maximum` - Worst-case across all tests (used for capacity metrics)
- `sum` - Total across all tests (used for fee metrics)
- `minimum`, `median` - Best-case and middle values
- `sum_positive`, `sum_negative` - Success vs failure cases

</template>

<template #5>

**Trade-offs & Variants**

- Optimizations involve trade-offs: size vs speed, memory vs CPU
- **Variants** allow submissions to indicate which side of a trade-off is optimized for
- Example: "size-optimized" vs "budget-optimized" variants

</template>
</v-switch>

<!--
**Raw Metrics:**

- "These are direct outputs from running UPLC on the CEK machine"
- "Script size: on-chain storage size in bytes"
- "Term size: AST node count, complexity indicator"

**Derived Metrics:**

- "These translate raw measurements into real-world impact"
- "Execution fee: 'How much does it cost to run this?'"
- "Budget utilization: 'What percentage of transaction/block limits does this consume?'"
- "Capacity: 'How many of these can fit in a transaction or block?'"
- "These are calculated using Conway mainnet protocol parameters"

**Key emphasis:**

- Aggregation strategies have semantic meaning - not arbitrary choices
- Trade-offs are real, variants capture different optimization goals

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
-->

---
layout: center
---

## Available Benchmarks: Two Key Dimensions

<v-switch :unmount="true">
<template #0>

<style>
.quadrant-grid { max-width: 450px; margin: 2rem auto 0; }
.quadrant-container { display: flex; gap: 1rem; align-items: center; }
.axis-label { color: #666; writing-mode: vertical-lr; transform: rotate(180deg); }
.axis-label-container { display: flex; flex-direction: column; justify-content: space-around; min-width: 80px; }
.grid-cell { padding: 1.2rem; display: flex; align-items: center; justify-content: center; text-align: center; }
.cell-text { font-size: 1.1em; font-weight: 500; line-height: 1.3; }
.x-axis-labels { display: flex; justify-content: space-around; margin-top: 0.5rem; color: #666; }
</style>

<div class="quadrant-grid">
  <div class="quadrant-container">
    <!-- Y-axis labels -->
    <div class="axis-label-container">
      <div class="axis-label" style="margin-bottom: 2rem;">Real-world&nbsp;→</div>
      <div class="axis-label" style="margin-top: auto;">← &nbsp;Synthetic</div>
    </div>
    <!-- Grid -->
    <div style="flex: 1;">
      <div style="display: grid; grid-template-columns: 1fr 1fr; grid-template-rows: 1fr 1fr; gap: 2px; background: #333; border: 2px solid #333; aspect-ratio: 1;">
        <div class="grid-cell" style="background: #e8f4f8;">
          <div class="cell-text" style="text-decoration: line-through;">Real-world<br/>Fixed Algo</div>
        </div>
        <div class="grid-cell" style="background: #f0e8f8;">
          <div class="cell-text">Real-world<br/>Any Algo</div>
        </div>
        <div class="grid-cell" style="background: #f8f0e8;">
          <div class="cell-text">Synthetic<br/>Fixed Algo</div>
        </div>
        <div class="grid-cell" style="background: #e8f8f0;">
          <div class="cell-text">Synthetic<br/>Any Algo</div>
        </div>
      </div>
      <!-- X-axis labels -->
      <div class="x-axis-labels">
        <span>← &nbsp;Fixed Algorithm</span>
        <span>Any Algorithm&nbsp;→</span>
      </div>
    </div>
  </div>
</div>

</template>

<template #1>

**Isolation dimension**

</template>

<template #2>

**Fixed Algorithm**

- **Purpose**: Isolate compiler optimization quality
- Examples:
  - Fibonacci via naive recursion `fibonacci_naive_recursion`
  - Factorial via naive recursion `factorial_naive_recursion`

</template>

<template #3>

**Open Algorithm**

- **Purpose**: Demonstrate both compiler and program optimizations E2E
- Examples:
  - Fibonacci `fibonacci`
  - Factorial `factorial`
  - Two-party Escrow validator `two_party_escrow`
- Submissions **converge on optimal implementation**<br>
  → &nbsp;showcases best practices

</template>

<template #4>

**Complexity dimension**

</template>

<template #5>

**Synthetic Scenarios** 

- Examples:
  - Fibonacci `fibonacci`
  - Factorial `factorial`
- Simple, avoid ScriptContext complexity
- Easier to contribute, faster to implement
- Less representative of real-world usage

</template>

<template #6>

**Real-World Scenarios** 

- Complex, use Plutus-Ledger API:
  - Data-encoded inputs
  - Uses `ScriptContext`
- Multi-transaction validator interactions (stateful)
- More educational and representative of production use

</template>

<template #7>

**Growing Set, Open for Contributions**

- Current scenarios are starting points
- We're actively expanding the set
- Community contributions welcome for both types

</template>
</v-switch>

<!--
**Key emphasis:**

- Two orthogonal dimensions create a matrix of scenario types
- Each type serves a different purpose
- Open scenarios naturally converge on optimal solutions across compilers

**Fixed Algorithm:**

- "These scenarios lock the algorithm everyone must use"
- "Example: fibonacci_naive_recursion forces naive recursive implementation"
- "All compilers implement the same approach → performance differences come purely from optimizer quality"
- "This isolates compiler optimization effectiveness"
- "You're testing: 'How good is your inlining? Your dead code elimination? Your constant folding?'"

**Open Algorithm:**

- "These allow any implementation approach"
- "Fibonacci can be implemented naively, iteratively, with memoization, closed-form, etc."
- "Authors compete and explore different strategies"
- "Over time, submissions converge on the optimal approach"
- "This convergence is valuable: it demonstrates best practices across the ecosystem"
- "Example: 'Here's how the best Plinth implementation looks, and here's the best Aiken implementation'"

**The convergence phenomenon:**

- "Open scenarios have a fascinating property"
- "Different compilers, different authors, same problem"
- "Over time: 'This is the best way to implement X in UPLC'"
- "Cross-pollination: techniques from one compiler inspire optimizations in another"
- "The benchmark becomes a repository of best practices"

**Key emphasis:**

- Complexity spectrum from simple to real-world
- Both types are valuable for different purposes

**Synthetic Scenarios:**

- "Simple algorithmic problems: fibonacci, factorial, sum, list operations"
- "No ScriptContext, no datum/redeemer complexity"
- "Easy to understand, quick to implement"
- "Great for getting started with CAPE"
- "But: not representative of real validators you'd deploy on mainnet"

**Real-World Scenarios:**

- "Actual validator patterns: vesting, escrow, multi-sig, token policies"
- "Multi-stage interactions: setup transaction → validator execution → cleanup"
- "Use plutus-ledger API, deal with ScriptContext"
- "More complex to contribute, but much more educational"
- "Representative of what you'd actually deploy"
- "These show real-world optimization techniques"

**Growing set:**

- "We're actively expanding the scenario set"
- "Planning to add more real-world scenarios"
- "Community contributions welcome - both synthetic and real-world"
- "If you have a validator pattern worth benchmarking, contribute it!"

**Transition:** "Now that you've seen what's available, let's talk about how you can create your own submission..."
-->

---
layout: center
---

## Creating Submissions: Step-by-Step

<v-switch :unmount="true">
<template #0>

**The Workflow**

1. **Understand the scenario** - Read spec, review test cases
1. **Create your project** - Separate GitHub repo or local project
1. **Initialize submission** - `cape submission new <scenario> <compiler> <version> <handle>`
1. **Fill in metadata** - References to source project, compiler details
1. **Write README** - Examples, build instructions, optimization notes
1. **Include UPLC file** - `<scenario>.uplc` compiled output
1. **Submit PR** - For community review

</template>

<template #1>

**1. Understand the scenario**
- Read specification in `scenarios/<name>/<name>.md`
- Review test cases in `cape-tests.json`
- Understand expected behavior and edge cases
</template>

<template #2>

**2. Create a submission(s) project**

Sources must be publishable for analysis, reproduction, forking

- Dedicated public GitHub repo
  - Example: `github.com/yourname/cape-submissions-mycompiler`
  - Sources will be **referenced** in submission PR,<br> 
    so a stable immutable reference (commit hash) is used.
- Local project
  - Project sources should be **included** in submission PR
</template>

<template #3>

**3. Initialize submission**
- `cape submission new <scenario> <compiler> <version> <handle> [variant]`
- Creates: `submissions/<scenario>/<Compiler>_<version>_<handle>[_variant]/`

</template>

<template #4>

**4. Fill in metadata** (`metadata.json`)
- Compiler name, version, contributor handle
- Source repository URL and commit hash
- Links your CAPE submission to your source project

</template>

<template #5>

**5. Write README**
- Explain your approach, optimizations, trade-offs
- Build instructions: how to reproduce the UPLC output
- Help others learn from your work

</template>

<template #6>

**6. Include UPLC file**
- Your compiled output: `<scenario>.uplc`
- Must match the scenario name

</template>

<template #7>

**7. Submit PR**
- Push your branch, create pull request
- Community review: sources, metadata, tests
- Collaborative feedback improves quality
</template>

<template #8>

**8. After merge**
- GitHub Actions runs `cape submission measure`
- Generates `metrics.json` automatically
- Updates HTML reports with your submission

</template>

<template #9>

**Common Gotcha: UPLC Names**

- Named variables in UPLC source `[a-zA-Z][a-zA-Z0-9_']*` 
- **Always test**: `cape submission verify <path>` before submitting
- Catches compatibility issues early

</template>
</v-switch>

<!--
**Key emphasis:**

- Submission is not just UPLC - it's sources, documentation, reproducibility
- The project lives outside CAPE repo, referenced by commit hash
- Community review is part of the process

**Workflow walkthrough:**

**1. Understand the scenario:**

- "Start by reading the scenario spec in `scenarios/<name>/<name>.md`"
- "Review `cape-tests.json` to see what test cases your implementation must handle"
- "Understand expected behavior: success cases, failure cases, edge cases"

**2. Create your project:**

- "This is YOUR project - it lives in its own repo or local directory"
- "Could be a GitHub repo: `github.com/yourname/cape-fibonacci-plinth`"
- "Or a local project you'll publish later"
- "Why separate? So others can fork it, improve it, learn from it"
- "Sources are critical - transparency and reproducibility"

**3. Complete & commit:**

- "Once your implementation works, commit it"
- "Get a stable commit hash - this becomes your immutable reference"
- "Example: `abc123def456` - this exact version produced these metrics"

**4. Initialize submission:**

- "`cape submission new fibonacci Plinth 1.11.0 myhandle`"
- "This creates the submission structure in CAPE repo"
- "Format: `submissions/fibonacci/Plinth_1.11.0_myhandle/`"

**5. Fill in metadata:**

- "Edit `metadata.json`"
- "Include: compiler name, version, contributor handle"
- "Source repository URL and commit hash"
- "This links your CAPE submission to your source project"

**6. Write README:**

- "Explain your approach, optimizations, trade-offs"
- "Build instructions: how to reproduce the UPLC output"
- "Examples of what makes this implementation interesting"
- "This is educational - help others learn from your work"

**7. Include UPLC file:**

- "Your compiled output: `fibonacci.uplc`"
- "This is what gets measured"
- "Must match the scenario name"

**8. Submit PR:**

- "Push your branch, create pull request"
- "Reviewers will check: sources make sense, metadata is correct, tests pass"
- "This is collaborative - feedback improves quality"

**9. After merge:**

- "GitHub Actions automatically runs `cape submission measure`"
- "Generates `metrics.json`"
- "Updates HTML reports with your submission"
- "Your results are now part of the benchmark"

**Common Gotcha: UPLC Names:**

- "UPLC de Bruijn indices can be fragile"
- "Named variables in source → nameless in UPLC"
- "Test your UPLC file against `cape-tests.json` BEFORE submitting"
- "Use `cape submission verify` to catch issues early"

**Why this workflow?**

- "Separating source project from CAPE submission keeps repos focused"
- "Commit hashes ensure reproducibility"
- "Community review maintains quality"
- "Published sources enable learning and improvement"

**Transition:** "After publishing a submission PR, where can you see the results?"
-->

---
layout: center
---

## Live Results & Community

<v-switch :unmount="true">

<template #0>

**Live Results**

- **URL**: https://intersectmbo.github.io/UPLC-CAPE/
- Also linked in repository sidebar
- **Auto-generated** static HTML site from measured metrics
- Updates automatically after PR merge via CI

</template>

<template #1>

**PR Preview Deployments**

- Submission PR authors get **preview reports**
- Published as PR comment
- See your results **before** merging
- Verify everything looks good before merging

</template>

<template #2>

**Community Resources**

- **Repository**: https://github.com/IntersectMBO/UPLC-CAPE
- **Issues**: Report bugs, request features, ask questions
- **Discussions**: Community chat, ideas, help
- **Documentation**: README, USAGE.md, CONTRIBUTING.md

</template>

<template #3>

**Get Involved**

- Submit benchmarks for your compiler
- Propose new scenarios
- Improve tooling and documentation
- Review submissions, share insights

</template>
</v-switch>

<!--
**Key emphasis:**

- Results are public, transparent, automatically updated
- Preview system lets you check before merging
- Multiple ways to engage with the community

**Live results walkthrough:**

**The website:**

- "All benchmark results are published at intersectmbo.github.io/UPLC-CAPE"
- "This is a static HTML site - no backend, no database"
- "Generated entirely from metrics.json files in the repo"
- "Tables and graphs for easy comparison"
- "You can see: CPU, memory, script size, fees, budget percentages, capacity"
- "Compare across compilers, across scenarios"

**Auto-generation:**

- "When a submission PR merges, GitHub Actions kicks in"
- "Runs cape submission measure, generates metrics.json"
- "Regenerates the entire HTML report"
- "Deploys to GitHub Pages"
- "Fully automated - no manual steps"

**PR preview deployments:**

- "Before your PR merges, you want to see results"
- "CI generates a preview deployment"
- "Posted as a comment on your PR"
- "Click the link, see your submission's metrics"
- "Compare against existing submissions"
- "This lets you verify everything looks good before merging"

**Community resources:**

**Repository:**

- "Main repo: github.com/IntersectMBO/UPLC-CAPE"
- "Browse scenarios, submissions, source code"
- "Fork it, clone it, explore it"

**Issues:**

- "Found a bug? Open an issue"
- "Want a new feature? Request it"
- "Stuck on something? Ask for help"

**Discussions:**

- "Less formal than issues"
- "Share ideas, ask questions, discuss approaches"
- "Community support"

**Documentation:**

- "README.md: Quick start guide"
- "USAGE.md: Complete CLI reference"
- "CONTRIBUTING.md: How to contribute"
- "doc/: In-depth documentation (metrics, domain model, etc.)"

**Get involved - specific calls to action:**

**1. Submit benchmarks:**

- "If you're a compiler author - submit benchmarks for your compiler"
- "Even if you're not competitive yet - baseline submissions are valuable"
- "Progress over time is interesting to track"

**2. Propose new scenarios:**

- "Have a validator pattern worth benchmarking?"
- "Real-world scenarios are especially valuable"
- "Open an issue to discuss, then contribute it"

**3. Improve tooling:**

- "The cape CLI is open source"
- "Improve error messages, add features, fix bugs"
- "Make it easier for the next contributor"

**4. Review submissions:**

- "Community review improves quality"
- "Check submissions make sense"
- "Share optimization insights"
- "Learn from others' approaches"

**Transition:** "Now let's get hands-on. Here's what we're going to do in the workshop portion..."
-->

---
layout: center
---

## Workshop: Hands-on Contribution

<v-switch :unmount="true">
<template #0>

**Choose Your Path**

- **Path 1: Simple Scenarios** - Beginners, learn submission process
- **Path 2: Real-World Scenario** - Advanced, validator patterns
- **Plinth Users** - In-repo convenience available
- **Support Available** - Help with submission process & language
- **Getting Started** - Clone, setup, implement

</template>

<template #1>

**Path 1: Simple Scenarios (Beginners)**

- Implement `factorial_naive_recursion`
- Use the language/compiler of your choice
- Focus: Learn the submission process without validator complexity

**Benefits:**
- Straightforward implementation
- No ScriptContext complexity
- Full end-to-end experience in 20-30 minutes

</template>

<template #2>

**Path 2: Real-World Scenario (Advanced)**

- Implement **Linear Vesting** validator
- Multi-stage validator interactions, ScriptContext handling
- Focus: Real-world validator patterns and optimizations

**Benefits:**
- Representative of production validators
- Learn real optimization techniques
- More educational and challenging

</template>

<template #3>

**Plinth Users: In-Repo Convenience**

- No separate project needed!
- Implement directly in UPLC-CAPE repo
- Structure already exists: `plinth-submissions-app/` and `lib/`
- See `cape.cabal` for examples

**For other languages:**
- You'll need your own project setup
- Compile to UPLC, bring the output file

</template>

<template #4>

**Support Available**

- UPLC-CAPE submission process help
- Plinth language guidance
- Troubleshooting and Q&A
- We'll circulate to help individuals

</template>

<template #5>

**Getting Started**

1. Clone repo: `git clone https://github.com/IntersectMBO/UPLC-CAPE`
2. Setup environment: `nix develop` (binary cache available)
3. Choose your path and scenario
4. Start implementing!

**Even if you don't finish:**
- You've learned the process
- Continue after the conference and submit a PR
- Goal: understand the workflow and get started

</template>
</v-switch>

<!--
**Key emphasis:**

- Two clear paths based on experience level
- Plinth has special convenience (in-repo development)
- I'm here to help with process and language questions

**Workshop structure:**

**Time allocation (25-30 minutes total):**

- Environment setup: 5 minutes
- Implementation: 15-20 minutes
- Validation & discussion: 5 minutes

**Path 1: Simple Scenarios**

**Who should choose this:**

- "If you've never implemented a Cardano validator"
- "If you want to focus on the submission process rather than validator complexity"
- "If you want a quick win to understand the full workflow"

**What you'll do:**

- "Pick fibonacci_naive_recursion or factorial_naive_recursion"
- "Implement it in your preferred language: Plinth, Aiken, OpShin, etc."
- "Follow the fixed algorithm specified in the scenario"
- "Compile to UPLC, create submission, verify it passes tests"

**Benefits:**

- "Straightforward implementation"
- "No ScriptContext complexity"
- "Full end-to-end experience in 20-30 minutes"
- "You'll understand the entire CAPE workflow"

**Path 2: Real-World Scenario (Linear Vesting)**

**Who should choose this:**

- "If you're comfortable with validators and plutus-ledger API"
- "If you want to tackle a real-world use case"
- "If you're ready for multi-transaction interactions"

**What you'll do:**

- "Implement a Linear Vesting validator"
- "Handle ScriptContext, datum/redeemer validation"
- "Abstract the off-chain parts (transaction building)"
- "Focus on the validator logic itself"

**Benefits:**

- "Representative of production validators"
- "Learn real optimization techniques"
- "More educational and challenging"

**Plinth in-repo convenience:**

**This is unique to Plinth:**

- "If you're using Plinth, you don't need a separate repo"
- "The UPLC-CAPE repo already has the project structure"
- "Look at `plinth-submissions-app/Main.hs` for the generator"
- "Look at `lib/` for existing implementations (Factorial, Fibonacci, TwoPartyEscrow)"
- "Add your implementation, compile, generate submission"
- "This is faster for the workshop setting"

**For other languages:**

- "You'll need your own project setup"
- "But that's realistic for real contributions"
- "Compile to UPLC, bring the output file"

**Support I'll provide:**

**UPLC-CAPE submission process:**

- "How to use `cape submission new`"
- "How to structure metadata.json"
- "How to run `cape submission verify`"
- "Common pitfalls and how to avoid them"

**Plinth language:**

- "Syntax questions"
- "How to use the fixtures system"
- "PlutusTx compilation pragmas"
- "Debugging UPLC output"

**Getting started:**

**Environment setup:**

- "git clone https://github.com/IntersectMBO/UPLC-CAPE"
- "cd UPLC-CAPE"
- "nix develop (binary cache is configured, should be fast)"
- "Verify: cape --version, cabal --version"

**Choose your path:**

- "Path 1: Simple → pick fibonacci_naive_recursion or factorial_naive_recursion"
- "Path 2: Advanced → linear vesting"

**Start implementing:**

- "I'll circulate to help individuals"
- "Ask questions as you go"
- "Share your progress, discuss approaches"

**End of workshop:**

- "Even if you don't finish, you've learned the process"
- "You can continue after the conference and submit a PR"
- "The goal is to understand the workflow and get started"

**Closing:**

- "Thank you for attending!"
- "Links: intersectmbo.github.io/UPLC-CAPE (results), github.com/IntersectMBO/UPLC-CAPE (repo)"
- "Let's build a comprehensive benchmark together!"
-->

---
layout: center
class: text-center
---

<div class="cover-logos">
  <img src="/uplc-logo.png" alt="UPLC Conference Logo" />
</div>

<div class="cover-content">
  <h1 class="cover-main">Thank You!</h1>
  <h1 class="cover-sub">Let's Build a Comprehensive</h1>
  <h1 class="cover-sub">Benchmark Together</h1>

  <p class="cover-date" style="margin-top: 3rem; font-size: 1.2em;">
    <strong>Live Results:</strong> https://intersectmbo.github.io/UPLC-CAPE/<br/>
    <strong>Repository:</strong> https://github.com/IntersectMBO/UPLC-CAPE
  </p>
</div>
