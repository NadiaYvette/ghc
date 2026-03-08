%global ghc_version 9.15.20260305
%global nadia_rel 1
%global install_prefix /opt/ghc-nadia
%global debug_package %{nil}

Name:           ghc-nadia
Version:        %{ghc_version}
Release:        %{nadia_rel}%{?dist}
Summary:        GHC %{ghc_version} with SSA register allocator and constraint solver optimizations
License:        BSD-3-Clause
URL:            https://github.com/NadiaYvette/ghc

Source0:        ghc-%{ghc_version}-x86_64-unknown-linux.tar.xz

BuildRequires:  make
BuildRequires:  gcc
BuildRequires:  gcc-c++
BuildRequires:  gmp-devel
BuildRequires:  ncurses-devel
BuildRequires:  libffi-devel

Requires:       gmp
Requires:       ncurses-libs
Requires:       libffi

# Don't conflict with Fedora's ghc package
Provides:       ghc-nadia = %{version}-%{release}

# Don't auto-provide/require from our private lib directory
%global __provides_exclude_from ^%{install_prefix}/.*$
%global __requires_exclude_from ^%{install_prefix}/.*$

%description
Custom GHC build from Nadia Chambers' development branch, installed to
%{install_prefix} to avoid conflicts with the system GHC.

Includes:
- SSA-based register allocator (-fregs-ssa)
- Indexed quantified constraints in the constraint solver
- Incremental kick-out via reverse dependency index
- Implication solving order optimization
- io_uring I/O manager for the RTS

%prep
%setup -q -n ghc-%{ghc_version}-x86_64-unknown-linux

%build
./configure --prefix=%{install_prefix}

%install
%make_install

# Create /etc/profile.d snippet for easy PATH setup
mkdir -p %{buildroot}%{_sysconfdir}/profile.d
cat > %{buildroot}%{_sysconfdir}/profile.d/ghc-nadia.sh << 'PROFILE'
# Add ghc-nadia to PATH (opt-in: source this or add to your shell config)
# Usage: source /etc/profile.d/ghc-nadia.sh
export PATH="%{install_prefix}/bin:$PATH"
PROFILE

# Create symlinks in /usr/local/bin with -nadia suffix
mkdir -p %{buildroot}/usr/local/bin
for prog in ghc ghci ghc-pkg haddock hsc2hs hp2ps runghc runhaskell; do
    if [ -f %{buildroot}%{install_prefix}/bin/$prog ]; then
        ln -sf %{install_prefix}/bin/$prog %{buildroot}/usr/local/bin/${prog}-nadia
    fi
done

%files
%{install_prefix}
%config(noreplace) %{_sysconfdir}/profile.d/ghc-nadia.sh
/usr/local/bin/*-nadia

%changelog
* Sat Mar 07 2026 Nadia Chambers <nadia.yvette.chambers@gmail.com> - 9.15.20260305-1
- Initial RPM build from nadia.chambers/incremental-simplifier branch
- SSA register allocator (-fregs-ssa)
- Constraint solver: indexed QCInsts, incremental kick-out, implication reordering
- io_uring I/O manager for non-threaded and threaded RTS
