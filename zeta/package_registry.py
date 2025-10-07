from __future__ import annotations
from dataclasses import dataclass, field
from typing import Dict, List, Set, Optional

from zeta.types.symbol import Symbol


@dataclass
class Package:
    name: str
    env: "Environment"
    exports: Set[str] = field(default_factory=set)  # store symbol ids
    use_list: List[str] = field(default_factory=list)  # list of package names
    aliases: Set[str] = field(default_factory=set)

    def export(self, *symbols: Symbol) -> None:
        for s in symbols:
            if isinstance(s, Symbol):
                self.exports.add(s.id)

    def uses(self, pkg_name: str) -> None:
        if pkg_name not in self.use_list:
            self.use_list.append(pkg_name)


class PackageRegistry:
    def __init__(self):
        self._packages: Dict[str, Package] = {}
        self._aliases: Dict[str, str] = {}

    def get(self, name_or_alias: str) -> Optional[Package]:
        real = self._aliases.get(name_or_alias, name_or_alias)
        return self._packages.get(real)

    def ensure(self, name: str, env: Optional["Environment"] = None) -> Package:
        pkg = self._packages.get(name)
        if pkg is None:
            if env is None:
                # Lazy import to avoid circular dependency at module load time
                from zeta.types.environment import Environment  # type: ignore
                env = Environment()
            pkg = Package(name=name, env=env)
            self._packages[name] = pkg
        return pkg

    def set_alias(self, alias: str, real_name: str) -> None:
        self._aliases[alias] = real_name

    def all(self) -> Dict[str, Package]:
        return self._packages


# Module-level singleton
_registry: Optional[PackageRegistry] = None

def get_registry() -> PackageRegistry:
    global _registry
    if _registry is None:
        _registry = PackageRegistry()
    return _registry
