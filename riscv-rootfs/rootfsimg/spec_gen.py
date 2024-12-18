import argparse
import os
import sys

elf_suffix = "_base.riscv64-linux-gnu-gcc-9.3.0"

# (filelist, arguments) information for each benchmark
# filelist[0] should always be the binary file
def get_spec_info():
  return {
  "astar_biglakes": (
    [
      "${SPEC}/spec06_exe/astar" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/astar/BigLakes2048.bin",
      "${SPEC}/cpu2006_run_dir/astar/BigLakes2048.cfg"
    ],
    [ "BigLakes2048.cfg" ],
    [ "int", "ref" ]
  ),
  "astar_rivers": (
    [
      "${SPEC}/spec06_exe/astar" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/astar/rivers.bin",
      "${SPEC}/cpu2006_run_dir/astar/rivers.cfg"
    ],
    [ "rivers.cfg" ],
    [ "int", "ref" ]
  ),
  "bwaves": (
    [
      "${SPEC}/spec06_exe/bwaves" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/bwaves/bwaves.in"
    ],
    [],
    [ "fp", "ref" ]
  ),
  "bzip2_chicken": (
    [
      "${SPEC}/spec06_exe/bzip2" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/bzip2/chicken.jpg"
    ],
    [ "chicken.jpg", "30" ],
    [ "int", "ref" ]
  ),
  "bzip2_combined": (
    [
      "${SPEC}/spec06_exe/bzip2" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/bzip2/input.combined"
    ],
    [ "input.combined", "200" ],
    [ "int", "ref" ]
  ),
  "bzip2_html": (
    [
      "${SPEC}/spec06_exe/bzip2" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/bzip2/text.html"
    ],
    [ "text.html", "280" ],
    [ "int", "ref" ]
  ),
  "bzip2_liberty": (
    [
      "${SPEC}/spec06_exe/bzip2" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/bzip2/liberty.jpg"
    ],
    [ "liberty.jpg", "30" ],
    [ "int", "ref" ]
  ),
  "bzip2_program": (
    [
      "${SPEC}/spec06_exe/bzip2" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/bzip2/input.program"
    ],
    [ "input.program", "280" ],
    [ "int", "ref" ]
  ),
  "bzip2_source": (
    [
      "${SPEC}/spec06_exe/bzip2" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/bzip2/input.source"
    ],
    [ "input.source", "280" ],
    [ "int", "ref" ]
  ),
  "cactusADM": (
    [
      "${SPEC}/spec06_exe/cactusADM" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/cactusADM/benchADM.par"
    ],
    [ "benchADM.par" ],
    [ "fp", "ref" ]
  ),
  "calculix": (
    [
      "${SPEC}/spec06_exe/calculix" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/calculix/hyperviscoplastic.dat",
      "${SPEC}/cpu2006_run_dir/calculix/hyperviscoplastic.frd",
      "${SPEC}/cpu2006_run_dir/calculix/hyperviscoplastic.inp",
      "${SPEC}/cpu2006_run_dir/calculix/hyperviscoplastic.sta"
    ],
    [ "-i", "hyperviscoplastic" ],
    [ "fp", "ref" ]
  ),
  "dealII": (
    [
      "${SPEC}/spec06_exe/dealII" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/dealII/DummyData"
    ],
    [ "23" ],
    [ "fp", "ref" ]
  ),
  "gamess_cytosine": (
    [
      "${SPEC}/spec06_exe/gamess" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/gamess/cytosine.2.config",
      "${SPEC}/cpu2006_run_dir/gamess/cytosine.2.inp"
    ],
    [ "<", "cytosine.2.config" ],
    [ "fp", "ref" ]
  ),
  "gamess_gradient": (
    [
      "${SPEC}/spec06_exe/gamess" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/gamess/h2ocu2+.gradient.config",
      "${SPEC}/cpu2006_run_dir/gamess/h2ocu2+.gradient.inp"
    ],
    [ "<", "h2ocu2+.gradient.config" ],
    [ "fp", "ref" ]
  ),
  "gamess_triazolium": (
    [
      "${SPEC}/spec06_exe/gamess" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/gamess/triazolium.config",
      "${SPEC}/cpu2006_run_dir/gamess/triazolium.inp"
    ],
    [ "<", "triazolium.config" ],
    [ "fp", "ref" ]
  ),
  "gcc_166": (
    [
      "${SPEC}/spec06_exe/gcc" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/gcc/166.i"
    ],
    [ "166.i", "-o", "166.s" ],
    [ "int", "ref" ]
  ),
  "gcc_200": (
    [
      "${SPEC}/spec06_exe/gcc" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/gcc/200.i"
    ],
    [ "200.i", "-o", "200.s" ],
    [ "int", "ref" ]
  ),
  "gcc_cpdecl": (
    [
      "${SPEC}/spec06_exe/gcc" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/gcc/cp-decl.i"
    ],
    [ "cp-decl.i", "-o", "cp-decl.s" ],
    [ "int", "ref" ]
  ),
  "gcc_expr2": (
    [
      "${SPEC}/spec06_exe/gcc" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/gcc/expr2.i"
    ],
    [ "expr2.i", "-o", "expr2.s" ],
    [ "int", "ref" ]
  ),
  "gcc_expr": (
    [
      "${SPEC}/spec06_exe/gcc" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/gcc/expr.i"
    ],
    [ "expr.i", "-o", "expr.s" ],
    [ "int", "ref" ]
  ),
  "gcc_g23": (
    [
      "${SPEC}/spec06_exe/gcc" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/gcc/g23.i"
    ],
    [ "g23.i", "-o", "g23.s" ],
    [ "int", "ref" ]
  ),
  "gcc_s04": (
    [
      "${SPEC}/spec06_exe/gcc" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/gcc/s04.i"
    ],
    [ "s04.i", "-o", "s04.s" ],
    [ "int", "ref" ]
  ),
  "gcc_scilab": (
    [
      "${SPEC}/spec06_exe/gcc" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/gcc/scilab.i"
    ],
    [ "scilab.i", "-o", "scilab.s" ],
    [ "int", "ref" ]
  ),
  "gcc_typeck": (
    [
      "${SPEC}/spec06_exe/gcc" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/gcc/c-typeck.i"
    ],
    [ "c-typeck.i", "-o", "c-typeck.s" ],
    [ "int", "ref" ]
  ),
  "GemsFDTD": (
    [
      "${SPEC}/spec06_exe/GemsFDTD" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/GemsFDTD/ref.in",
      "${SPEC}/cpu2006_run_dir/GemsFDTD/sphere.pec",
      "${SPEC}/cpu2006_run_dir/GemsFDTD/yee.dat"
    ],
    [],
    [ "fp", "ref" ]
  ),
  "gobmk_13x13": (
    [
      "${SPEC}/spec06_exe/gobmk" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/gobmk/13x13.tst",
      "dir games /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/gobmk/games",
      "dir golois /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/gobmk/golois"
    ],
    [ "--quiet", "--mode", "gtp", "<", "13x13.tst" ],
    [ "int", "ref" ]
  ),
  "gobmk_nngs": (
    [
      "${SPEC}/spec06_exe/gobmk" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/gobmk/nngs.tst",
      "dir games /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/gobmk/games",
      "dir golois /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/gobmk/golois"
    ],
    [ "--quiet", "--mode", "gtp", "<", "nngs.tst" ],
    [ "int", "ref" ]
  ),
  "gobmk_score2": (
    [
      "${SPEC}/spec06_exe/gobmk" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/gobmk/score2.tst",
      "dir games /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/gobmk/games",
      "dir golois /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/gobmk/golois"
    ],
    [ "--quiet", "--mode", "gtp", "<", "score2.tst" ],
    [ "int", "ref" ]
  ),
  "gobmk_trevorc": (
    [
      "${SPEC}/spec06_exe/gobmk" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/gobmk/trevorc.tst",
      "dir games /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/gobmk/games",
      "dir golois /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/gobmk/golois"
    ],
    [ "--quiet", "--mode", "gtp", "<", "trevorc.tst" ],
    [ "int", "ref" ]
  ),
  "gobmk_trevord": (
    [
      "${SPEC}/spec06_exe/gobmk" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/gobmk/trevord.tst",
      "dir games /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/gobmk/games",
      "dir golois /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/gobmk/golois"
    ],
    [ "--quiet", "--mode", "gtp", "<", "trevord.tst" ],
    [ "int", "ref" ]
  ),
  "gromacs": (
    [
      "${SPEC}/spec06_exe/gromacs" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/gromacs/gromacs.tpr"
    ],
    [ "-silent", "-deffnm", "gromacs.tpr", "-nice", "0" ],
    [ "fp", "ref" ]
  ),
  "h264ref_foreman.baseline": (
    [
      "${SPEC}/spec06_exe/h264ref" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/h264ref/foreman_ref_encoder_baseline.cfg",
      "${SPEC}/cpu2006_run_dir/h264ref/foreman_qcif.yuv"
    ],
    [ "-d", "foreman_ref_encoder_baseline.cfg" ],
    [ "int", "ref" ]
  ),
  "h264ref_foreman.main": (
    [
      "${SPEC}/spec06_exe/h264ref" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/h264ref/foreman_ref_encoder_main.cfg",
      "${SPEC}/cpu2006_run_dir/h264ref/foreman_qcif.yuv"
    ],
    [ "-d", "foreman_ref_encoder_main.cfg" ],
    [ "int", "ref" ]
  ),
  "h264ref_sss": (
    [
      "${SPEC}/spec06_exe/h264ref" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/h264ref/sss_encoder_main.cfg",
      "${SPEC}/cpu2006_run_dir/h264ref/sss.yuv"
    ],
    [ "-d", "sss_encoder_main.cfg" ],
    [ "int", "ref" ]
  ),
  "hmmer_nph3": (
    [
      "${SPEC}/spec06_exe/hmmer" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/hmmer/nph3.hmm",
      "${SPEC}/cpu2006_run_dir/hmmer/swiss41"
    ],
    [ "nph3.hmm", "swiss41" ],
    [ "int", "ref" ]
  ),
  "hmmer_retro": (
    [
      "${SPEC}/spec06_exe/hmmer" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/hmmer/retro.hmm"
    ],
    [ "--fixed", "0", "--mean", "500", "--num", "500000", "--sd", "350", "--seed", "0", "retro.hmm" ],
    [ "int", "ref" ]
  ),
  "lbm": (
    [
      "${SPEC}/spec06_exe/lbm" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/lbm/100_100_130_ldc.of",
      "${SPEC}/cpu2006_run_dir/lbm/lbm.in"
    ],
    [ "3000", "reference.dat", "0", "0", "100_100_130_ldc.of" ],
    [ "fp", "ref" ]
  ),
  "leslie3d": (
    [
      "${SPEC}/spec06_exe/leslie3d" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/leslie3d/leslie3d.in"
    ],
    [ "<", "leslie3d.in" ],
    [ "fp", "ref" ]
  ),
  "libquantum": (
    [
      "${SPEC}/spec06_exe/libquantum" + elf_suffix
    ],
    [ "1397", "8" ],
    [ "int", "ref" ]
  ),
  "mcf": (
    [
      "${SPEC}/spec06_exe/mcf" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/mcf/inp.in"
    ],
    [ "inp.in" ],
    [ "int", "ref" ]
  ),
  "milc": (
    [
      "${SPEC}/spec06_exe/milc" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/milc/su3imp.in"
    ],
    [ "<", "su3imp.in" ],
    [ "fp", "ref" ]
  ),
  "namd": (
    [
      "${SPEC}/spec06_exe/namd" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/namd/namd.input"
    ],
    [ "--input", "namd.input", "--iterations", "38", "--output", "namd.out" ],
    [ "fp", "ref" ]
  ),
  "omnetpp": (
    [
      "${SPEC}/spec06_exe/omnetpp" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/omnetpp/omnetpp.ini"
    ],
    [ "omnetpp.ini" ],
    [ "int", "ref" ]
  ),
  "perlbench_checkspam": (
    [
      "${SPEC}/spec06_exe/perlbench" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/perlbench/cpu2006_mhonarc.rc",
      "${SPEC}/cpu2006_run_dir/perlbench/checkspam.pl",
      "${SPEC}/cpu2006_run_dir/perlbench/checkspam.in",
      "dir lib /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/perlbench/lib",
      "dir rules /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/perlbench/rules"
    ],
    [ "-I./lib", "checkspam.pl", "2500", "5", "25", "11", "150", "1", "1", "1", "1" ],
    [ "int", "ref" ]
  ),
  "perlbench_diffmail": (
    [
      "${SPEC}/spec06_exe/perlbench" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/perlbench/cpu2006_mhonarc.rc",
      "${SPEC}/cpu2006_run_dir/perlbench/diffmail.pl",
      "${SPEC}/cpu2006_run_dir/perlbench/diffmail.in",
      "dir lib /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/perlbench/lib",
      "dir rules /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/perlbench/rules"
    ],
    [ "-I./lib", "diffmail.pl", "4", "800", "10", "17", "19", "300" ],
    [ "int", "ref" ]
  ),
  "perlbench_splitmail": (
    [
      "${SPEC}/spec06_exe/perlbench" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/perlbench/cpu2006_mhonarc.rc",
      "${SPEC}/cpu2006_run_dir/perlbench/splitmail.pl",
      "${SPEC}/cpu2006_run_dir/perlbench/splitmail.in",
      "dir lib /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/perlbench/lib",
      "dir rules /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/perlbench/rules"
    ],
    [ "-I./lib", "splitmail.pl", "1600", "12", "26", "16", "4500" ],
    [ "int", "ref" ]
  ),
  "povray": (
    [
      "${SPEC}/spec06_exe/povray" + elf_suffix,
      "dir . /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/povray"
    ],
    [ "SPEC-benchmark-ref.ini" ],
    [ "fp", "ref" ]
  ),
  "sjeng": (
    [
      "${SPEC}/spec06_exe/sjeng" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/sjeng/ref.txt"
    ],
    [ "ref.txt" ],
    [ "int", "ref" ]
  ),
  "soplex_pds-50": (
    [
      "${SPEC}/spec06_exe/soplex" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/soplex/pds-50.mps"
    ],
    [ "-s1", "-e", "-m45000", "pds-50.mps" ],
    [ "fp", "ref" ]
  ),
  "soplex_ref": (
    [
      "${SPEC}/spec06_exe/soplex" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/soplex/ref.mps"
    ],
    [ "-m3500", "ref.mps" ],
    [ "fp", "ref" ]
  ),
  "sphinx3": (
    [
      "${SPEC}/spec06_exe/sphinx3" + elf_suffix,
      "dir . /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/sphinx3"
    ],
    [ "ctlfile", ".", "args.an4" ],
    [ "fp", "ref" ]
  ),
  "tonto": (
    [
      "${SPEC}/spec06_exe/tonto" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/tonto/stdin"
    ],
    [],
    [ "fp", "ref" ]
  ),
  "wrf": (
    [
      "${SPEC}/spec06_exe/wrf" + elf_suffix,
      "dir . /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/wrf"
    ],
    [],
    [ "fp", "ref" ]
  ),
  "xalancbmk": (
    [
      "${SPEC}/spec06_exe/xalancbmk" + elf_suffix,
      "dir . /nfs/home/share/xs-workloads/spec/spec-all/cpu2006_run_dir/xalancbmk"
    ],
    [ "-v", "t5.xml", "xalanc.xsl" ],
    [ "int", "ref" ]
  ),
  "zeusmp": (
    [
      "${SPEC}/spec06_exe/zeusmp" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/zeusmp/zmp_inp"
    ],
    [],
    [ "fp", "ref" ]
  ),
  # WARNING: this is SPEC test
  "gamess_exam29": (
    [
      "${SPEC}/spec06_exe/gamess" + elf_suffix,
      "${SPEC}/cpu2006_run_dir/gamess/exam29.config",
      "${SPEC}/cpu2006_run_dir/gamess/exam29.inp"
    ],
    [ "<", "exam29.config" ],
    ["fp", "test"]
  ),
}

default_files = [
  "dir /bin 755 0 0",
  "dir /etc 755 0 0",
  "dir /dev 755 0 0",
  "dir /lib 755 0 0",
  "dir /proc 755 0 0",
  "dir /sbin 755 0 0",
  "dir /sys 755 0 0",
  "dir /tmp 755 0 0",
  "dir /usr 755 0 0",
  "dir /mnt 755 0 0",
  "dir /usr/bin 755 0 0",
  "dir /usr/lib 755 0 0",
  "dir /usr/sbin 755 0 0",
  "dir /var 755 0 0",
  "dir /var/tmp 755 0 0",
  "dir /root 755 0 0",
  "dir /var/log 755 0 0",
  "",
  "nod /dev/console 644 0 0 c 5 1",
  "nod /dev/null 644 0 0 c 1 3",
  "",
  "# libraries",
  "file /lib/ld-linux-riscv64-lp64d.so.1 ${RISCV}/sysroot/lib/ld-linux-riscv64-lp64d.so.1 755 0 0",
  "file /lib/libc.so.6 ${RISCV}/sysroot/lib/libc.so.6 755 0 0",
  "file /lib/libresolv.so.2 ${RISCV}/sysroot/lib/libresolv.so.2 755 0 0",
  "file /lib/libm.so.6 ${RISCV}/sysroot/lib/libm.so.6 755 0 0",
  "file /lib/libdl.so.2 ${RISCV}/sysroot/lib/libdl.so.2 755 0 0",
  "file /lib/libpthread.so.0 ${RISCV}/sysroot/lib/libpthread.so.0 755 0 0",
  "",
  "# busybox",
  "file /bin/busybox ${RISCV_ROOTFS_HOME}/rootfsimg/build/busybox 755 0 0",
  "file /etc/inittab ${RISCV_ROOTFS_HOME}/rootfsimg/inittab-spec 755 0 0",
  "slink /init /bin/busybox 755 0 0",
  "",
  "# SPEC common",
  "dir /spec_common 755 0 0",
  "file /spec_common/before_workload ${SPEC}/before_workload 755 0 0",
  "file /spec_common/trap ${SPEC}/trap_new 755 0 0",
  "",
  "# SPEC",
  "dir /spec 755 0 0",
  "file /spec/run.sh ${RISCV_ROOTFS_HOME}/rootfsimg/run.sh 755 0 0"
]

def traverse_path(path, stack=""):
  all_dirs, all_files = [], []
  for item in os.listdir(path):
    item_path = os.path.join(path, item)
    item_stack = os.path.join(stack, item)
    if os.path.isfile(item_path):
      all_files.append(item_stack)
    else:
      all_dirs.append(item_stack)
      sub_dirs, sub_files = traverse_path(item_path, item_stack)
      all_dirs.extend(sub_dirs)
      all_files.extend(sub_files)
  return (all_dirs, all_files)

def generate_initramfs(specs):
  lines = default_files.copy()
  for spec in specs:
    spec_files = get_spec_info()[spec][0]
    for i, filename in enumerate(spec_files):
      if len(filename.split()) == 1:
        # print(f"default {filename} to file 755 0 0")
        basename = filename.split("/")[-1]
        filename = f"file /spec/{basename} {filename} 755 0 0"
        lines.append(filename)
      elif len(filename.split()) == 3:
        node_type, name, path = filename.split()
        if node_type != "dir":
          print(f"unknown filename: {filename}")
          continue
        all_dirs, all_files = traverse_path(path)
        lines.append(f"dir /spec/{name} 755 0 0")
        for sub_dir in all_dirs:
          lines.append(f"dir /spec/{name}/{sub_dir} 755 0 0")
        for file in all_files:
          lines.append(f"file /spec/{name}/{file} {path}/{file} 755 0 0")
      else:
        print(f"unknown filename: {filename}")
  with open("initramfs-spec.txt", "w") as f:
    f.writelines(map(lambda x: x + "\n", lines))


def generate_run_sh(specs, withTrap=False):
  lines =[ ]
  lines.append("#!/bin/sh")
  lines.append("echo '===== Start running SPEC2006 ====='")
  for spec in specs:
    spec_bin = get_spec_info()[spec][0][0].split("/")[-1]
    spec_cmd = " ".join(get_spec_info()[spec][1])
    lines.append(f"echo '======== BEGIN {spec} ========'")
    lines.append("set -x")
    lines.append(f"md5sum /spec/{spec_bin}")
    lines.append("date -R")
    if withTrap:
      lines.append("/spec_common/before_workload")
    lines.append(f"cd /spec && ./{spec_bin} {spec_cmd}")
    if withTrap:
      lines.append("/spec_common/trap")
    lines.append("date -R")
    lines.append("set +x")
    lines.append(f"echo '======== END   {spec} ========'")
  lines.append("echo '===== Finish running SPEC2006 ====='")
  with open("run.sh", "w") as f:
    f.writelines(map(lambda x: x + "\n", lines))

def generate_build_scripts(specs, withTrap=False, spec_gen=__file__):
  lines = []
  lines.append("#!/bin/sh")
  lines.append("set -x")
  lines.append("set -e")
  spike_dir, linux_dir = "../../riscv-pk", "../../riscv-linux"
  lines.append("mkdir -p spec_images")
  for spec in specs:
    target_dir = f"spec_images/{spec}"
    lines.append(f"mkdir -p {target_dir}")
    extra_args = ""
    if withTrap:
      extra_args += " --checkpoints"
    extra_args += f" --elf-suffix {elf_suffix}"
    lines.append(f"python3 {spec_gen} {spec}{extra_args}")
    lines.append(f"make -s -C {spike_dir} clean && make -s -C {spike_dir} -j100")
    bbl_elf = f"{spike_dir}/build/bbl"
    linux_elf = f"{linux_dir}/vmlinux"
    spec_elf = get_spec_info()[spec][0][0]
    bbl_bin = f"{spike_dir}/build/bbl.bin"
    for f in [bbl_elf, linux_elf, spec_elf]:
      filename = os.path.basename(f)
      lines.append(f"riscv64-unknown-linux-gnu-objdump -d {f} > {target_dir}/{filename}.txt")
    for f in [bbl_elf, linux_elf, spec_elf, bbl_bin]:
      lines.append(f"cp {f} {target_dir}")
  with open("build.sh", "w") as f:
    f.writelines(map(lambda x: x + "\n", lines))

if __name__ == "__main__":
  parser = argparse.ArgumentParser(description='CPU CPU2006 ramfs scripts')
  parser.add_argument('benchspec', nargs='*', help='selected benchmarks')
  parser.add_argument('--elf-suffix', '-s',
                      help='elf suffix (default: _base.riscv64-linux-gnu-gcc-9.3.0)')
  parser.add_argument('--checkpoints', action='store_true',
                      help='checkpoints mode (with before_workload and trap)')
  parser.add_argument('--scripts', action='store_true',
                      help='generate build scripts for spec ramfs')

  args = parser.parse_args()

  if args.elf_suffix is not None:
    elf_suffix = args.elf_suffix

  # parse benchspec
  benchspec = []
  spec_info = get_spec_info()
  for s in args.benchspec:
    if s in spec_info:
      benchspec.append(s)
    else:
      benchspec += [k for k in spec_info.keys() if set(s.split(",")) <= set(spec_info[k][2])]
  benchspec = sorted(set(benchspec))
  print(f"All {len(benchspec)} selected benchspec: {' '.join(benchspec)}")

  if args.scripts:
    generate_build_scripts(benchspec, args.checkpoints)
  else:
    generate_initramfs(benchspec)
    generate_run_sh(benchspec, args.checkpoints)
