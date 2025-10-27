import{_ as s}from"./slidev/VSwitch-CVQdUm0s.js";import{b as u,o as p,w as t,g as n,e as c,i as d,A as l,v as m,x as h,B as o}from"./modules/vue-Bgke8tQ4.js";import{I as g}from"./slidev/center-drVaqJW4.js";import{u as f,f as v}from"./slidev/context-CIjXkXHK.js";import"./modules/shiki-mDVCqiSy.js";import"./index-DuMeNKXT.js";const L={__name:"slides.md__slidev_21",setup(P){const{$clicksContext:i,$frontmatter:a}=f();return i.setup(),(b,e)=>{const r=s;return p(),u(g,m(h(o(v)(o(a),20))),{default:t(()=>[e[6]||(e[6]=n("h2",null,"Workshop: Hands-on Contribution",-1)),c(r,{unmount:!0},{0:t(()=>[...e[0]||(e[0]=[n("p",null,[n("strong",null,"Choose Your Path")],-1),n("ul",null,[n("li",null,[n("strong",null,"Path 1: Simple Scenarios"),l(" - Beginners, learn submission process")]),n("li",null,[n("strong",null,"Path 2: Real-World Scenario"),l(" - Advanced, validator patterns")]),n("li",null,[n("strong",null,"Plinth Users"),l(" - In-repo convenience available")]),n("li",null,[n("strong",null,"Support Available"),l(" - Help with submission process & language")]),n("li",null,[n("strong",null,"Getting Started"),l(" - Clone, setup, implement")])],-1)])]),1:t(()=>[...e[1]||(e[1]=[n("p",null,[n("strong",null,"Path 1: Simple Scenarios (Beginners)")],-1),n("ul",null,[n("li",null,[l("Implement "),n("code",null,"factorial_naive_recursion")]),n("li",null,"Use the language/compiler of your choice"),n("li",null,"Focus: Learn the submission process without validator complexity")],-1),n("p",null,[n("strong",null,"Benefits:")],-1),n("ul",null,[n("li",null,"Straightforward implementation"),n("li",null,"No ScriptContext complexity"),n("li",null,"Full end-to-end experience in 20-30 minutes")],-1)])]),2:t(()=>[...e[2]||(e[2]=[n("p",null,[n("strong",null,"Path 2: Real-World Scenario (Advanced)")],-1),n("ul",null,[n("li",null,[l("Implement "),n("strong",null,"Two-Party Escrow"),l(" validator")]),n("li",null,"Multi-stage validator interactions, ScriptContext handling"),n("li",null,"Focus: Real-world validator patterns and optimizations")],-1),n("p",null,[n("strong",null,"Benefits:")],-1),n("ul",null,[n("li",null,"Representative of production validators"),n("li",null,"Learn real optimization techniques"),n("li",null,"More educational and challenging")],-1)])]),3:t(()=>[...e[3]||(e[3]=[n("p",null,[n("strong",null,"Plinth Users: In-Repo Convenience")],-1),n("ul",null,[n("li",null,"No separate project needed!"),n("li",null,"Implement directly in UPLC-CAPE repo"),n("li",null,[l("Structure already exists: "),n("code",null,"plinth-submissions-app/"),l(" and "),n("code",null,"lib/")]),n("li",null,[l("See "),n("code",null,"cape.cabal"),l(" for examples")])],-1),n("p",null,[n("strong",null,"For other languages:")],-1),n("ul",null,[n("li",null,"You’ll need your own project setup"),n("li",null,"Compile to UPLC, bring the output file")],-1)])]),4:t(()=>[...e[4]||(e[4]=[n("p",null,[n("strong",null,"Support Available")],-1),n("ul",null,[n("li",null,"UPLC-CAPE submission process help"),n("li",null,"Plinth language guidance"),n("li",null,"Troubleshooting and Q&A"),n("li",null,"We’ll circulate to help individuals")],-1)])]),5:t(()=>[...e[5]||(e[5]=[n("p",null,[n("strong",null,"Getting Started")],-1),n("ol",null,[n("li",null,[l("Clone repo: "),n("code",null,"git clone https://github.com/IntersectMBO/UPLC-CAPE")]),n("li",null,[l("Set up the environment: "),n("code",null,"nix develop"),l(" (binary cache available)")]),n("li",null,"Choose your path and scenario"),n("li",null,"Start implementing!")],-1),n("p",null,[n("strong",null,"Even if you don’t finish:")],-1),n("ul",null,[n("li",null,"You’ve learned the process"),n("li",null,"Continue after the conference and submit a PR"),n("li",null,"Goal: understand the workflow and get started")],-1)])]),_:1}),d(`
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
- "Look at \`plinth-submissions-app/Main.hs\` for the generator"
- "Look at \`lib/\` for existing implementations (Factorial, Fibonacci, TwoPartyEscrow)"
- "Add your implementation, compile, generate submission"
- "This is faster for the workshop setting"

**For other languages:**

- "You'll need your own project setup"
- "But that's realistic for real contributions"
- "Compile to UPLC, bring the output file"

**Support I'll provide:**

**UPLC-CAPE submission process:**

- "How to use \`cape submission new\`"
- "How to structure metadata.json"
- "How to run \`cape submission verify\`"
- "Common pitfalls and how to avoid them"

**Plinth language:**

- "Syntax questions"
- "How to use the fixtures system"
- "PlutusTx compilation pragmas"
- "Debugging UPLC output"

**Getting started:**

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
`),e[7]||(e[7]=n("ul",null,[n("li",null,'"Let’s build a comprehensive benchmark together!" –>')],-1))]),_:1},16)}}};export{L as default};
