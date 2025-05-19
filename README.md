# Generally Follow the Guide But at the time of this fork there were issues with my build i had to do some rough patches
System: 
I tried Ubuntu intitially and had errors, i thought i probably needed to stick faithfully to the gcc/g++ version, installing old gcc/g++ was giving me a headache when i tried building from source 
1. RHEL 7
2. g++ 7.3, clang 5.0.2, gcc 7.3 according to the actual guide requirements
3. My pc crashed several times when building on the latest version of ubuntu, i dont know why 

## Issues i patched
1. I was getting errors on a construct Dynamorio was using, i believe this may have being due to a recent change that is not faithful to the C/C++ standard specified in scarab's makefiles.
   - I modified the construct to what is required by the old standard. This is in scarab/src/deps/dynamorio/clients/drcachesim/common/trace_entry.h.
  
3. Scarab uses boost libraries(they used a multi index container somewhere that is not provided by stock STL), i had to install libbbost, if u have it by default should not be a problem.

**Not sure about 4, the source files work fine when i built in ubuntu 18.04 without necessitating this refactor, i believe it has to do with the version of libbost, 
i think newer libboost versions enforce the rules that  trigger this error, the libboost that ships with ubuntu 18.04 doesnot have this error**
4. There is an issue with the multi container used by scarab, i had to do a rough patch that may not be consistent with the original intent of the authors. 
   - The multi index container records as entry a PredictorEntry object
   - PredictorEntry class has  member object Predictor States among other data members. 
   - PredictorStates deletes its copy constructor but retains its move constructor, this is so because it has unique_ptr member objects and thus is not copyable by default.
   - Since PredictorStates is not copyable, PredictorEntry isnt copyable too. 
   - The multiindex container from boost is utilised to create a table with PredictorEntry object entries.
   - The insert function of the container is used to add new elments but it seems, internally the insert function uses the copy constructor of the template its instantiated with, in this case PredictorEntry
   - Since PredictorEntry is not copy constructible, the boost container throws exceptions and thus the tool wont build
   - The error can be resolved by definning a copy constructor for predictorStates and some minor edits to the constructor of PredictorEntry, which i do in this fork
   - While the unique_ptr object compels only move operations it could be that it was intentionally used to optimise away the overhead of copying during checkpointing of the predictor
   - In that case a more critical look of the logic is required to make things work while maintaining the move sematics
   - If checkpointing was not the intent but rather, the error is induced by the unique_ptr only and defining a copy constructor wont break the desired the functionality then my solution suffices.
   - In path scarab/src/bp/cbp_tagescl_64k.h
5. In scarab/src/libs/cache_lib.c, line 1515 had to define RRIP_M as a symbolic constant cos it was declared intitially as const and used to evaluate another const static variable, it seems the standard does not allow this. 
6. In scarab/src/node_issue_queue.cc, line 187 and 192 had to make some minor edits, remove braces of the rhs value 



7. My distro does not provide snappy-devel(libsnappy) and libconfig++ thus the build failed to link this unless they were provided as git submodules that can be installed by the command git submodules init I had to install them manually and add add them to the linker path.
8. **Apparently it all comes down building scarab in an environent that matches the initial build, in that case the failures are trivial to fix.**
9. scarab requires, libbost, cmake, libsnappy-dev and libconfig-dev. In addition build in Ubuntu 18.04, have gcc/g++ 7.4 and clang 5/6 for the best results


# Scarab Quick Start Guide
Install:
1. Install exact PIN version ([PIN 3.15](https://www.intel.com/content/www/us/en/developer/articles/tool/pin-a-binary-instrumentation-tool-downloads.html))
2. Export the following paths
  - `export PIN_ROOT=/path/to/pin-3.15`
  - `export SCARAB_ENABLE_PT_MEMTRACE=1`
3. `cd src && make`

Run:
1. Copy: `src/PARAMS.sunny_cove` into your run directory and rename to `PARAMS.in`
2. Run: `src/scarab --frontend memtrace --cbp_trace_r0=<MEMTRACE_FILE>`

# Scarab

Scarab is a cycle accurate simulator for state-of-the-art, high performance,
multicore chips. Scarab's goal is to be highly accurate, while also being
fast and easy to work with.

##### Simulator Features:
* Accurate: Scarab is detailed cycle accurate uArchitecture model
* Fast: 600 KIPS trace-driven, 100 KIPS exec-driven
* SimPoint Support: Checkpoints, Fast-Forward, Marker Instructions
* Execute-at-Fetch: Easier support for oracle features, faster development of new features

##### v.2.0 Release Features:
* Support for Dynomrio Memtrace and Intel Processor Trace (PT) frontends
* Wrong-path execution for trace-based frontends (instruction replay)

##### What Code Can Scarab Run?
* Single-threaded x86\_64 programs that can be run on Intel's [PIN](https://software.intel.com/en-us/articles/pin-a-dynamic-binary-instrumentation-tool)

##### Scarab uArchitecture:
* All typical pipeline stages and out-of-order structures (Fetch, Decode, Rename, Retire, ROB, R/S, and more...)
* Multicore 
* Wrong path simulation
* Cache Hierarchy (Private L1, Private MLC, Private/Shared LLC)
* Ramulator Memory Simulator (DDR3/4, LPDDR3/4, GDDR5, HBM, WideIO/2, and more...)  
* Interface to McPat and CACTI for system level power/energy modeling
* Support for DVFS
* Latest Branch Predictors and Data Prefetchers (TAGE-SC-L, Stride, Stream, 2dc, GHB, Markov, and more...)

##### v.2.0 uArchitecture Extensions:
* Decoupled Frontend
* Micro-op Cache
* Register Renaming (limited GRF/rename stalls)
* Updated branch predictor and increased recovery accuracy
* FDIP/UDP prefetcher

##### Code Limitations
* 32-bit binaries not supported (work in progress)
* Performance of System Code not modeled
* No cooperative multithreaded code

##### uArch Limitations
* No SMT
* No real OS virtual to physical address translation
* Shared bus interconnect only (ring, mesh, and others are in progress.)

##### Credits 
Scarab was created in collaboration with HPS and SAFARI. This project was sponsored by Intel Labs.
Scarab v.2.0 was created and is currently maintained by UCSC.

The Scarab v.2.0 artifact is the result of our UDP ISCA 2024 paper. If you are using Scarab v.2.0 in your research please cite:

```
@inproceedings{oh2024udp,
  author = {Oh, Surim and Xu, Mingsheng and Khan, Tanvir Ahmed and Kasikci, Baris and Litz, Heiner},
  title = {UDP: Utility-Driven Fetch Directed Instruction Prefetching},
  booktitle = {Proceedings of the 51st International Symposium on Computer Architecture (ISCA)},
  series = {ISCA 2024},
  year = {2024},
  month = jun,
}
```

## License & Copyright
Please see the [LICENSE](LICENSE) for more information.

## Getting Started

1. [System requirements and software prerequisites.](docs/system_requirements.md)
2. [Compiling Scarab.](docs/compiling-scarab.md)
3. [Setting up and running auto-verification on Scarab.](docs/verification.md)
4. Running a single program on Scarab.
5. Running multiple jobs locally or on a batch system. (coming soon!)
6. Viewing batch job status and results. (coming soon!)
7. [Simulating dynamorio memtraces](docs/memtrace.md)
8. Solutions to common Scarab problems.

## Contributing to Scarab

Found a bug? [File a bug report.](https://github.com/hpsresearchgroup/scarab/issues/new/choose)

Request a new feature? [File a feature request.](https://github.com/hpsresearchgroup/scarab/issues/new/choose)

Have code you would like to commit? [Create a pull request.](https://github.com/hpsresearchgroup/scarab/pulls)

## Other Resources


1) Auto-generated software documentation can be found [here](docs/doxygen/index.html).

* Please run this command in this directory to auto-generate documentation files.
> make -C docs
