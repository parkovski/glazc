#include "glaz.h"
#include "ast.h"
#include <sstream>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/case_conv.hpp>

using namespace glaz;

bool Type::isVoid() const {
	return typeClass() == INTRINSIC &&
		static_cast<const IntrinsicType *>(this)->getIntrinsicId() ==
		IntrinsicType::VOID;
}

const PointerType *Type::getPtrType() const {
	if (!ptrtome)
		ptrtome = new PointerType(this);
	return ptrtome;
}

namespace {
	const char *const intrinsic_names[] = {
		"void", "bool", "char", "schar",
		"word", "sword", "uint", "int",
		"uint64", "int64", "float", "double"
	};
}

IntrinsicType::IntrinsicType(IntrinsicId which) :
	Type(),
	which(which),
	name(intrinsic_names[which]),
	cached(0) { }

bool IntrinsicType::operator==(const Type &rhs) const {
	if (rhs.typeClass() != INTRINSIC)
		return false;
	
	// Intrinsic type compare is easy: test the int values.
	return which == static_cast<const IntrinsicType &>(rhs).which;
}

Struct::~Struct() {
}

bool Struct::operator==(const Type &rhs) const {
	if (rhs.typeClass() != STRUCT)
		return false;
	
	const Struct &other = static_cast<const Struct &>(rhs);
	
	if (alignment != other.alignment)
		return false;
	
	// If they are not implemented yet, depend on names to tell them apart
	if (!is_implemented || !other.is_implemented) {
		return boost::iequals(name, other.name);
	}
	
	var_list::const_iterator left = vars.begin(),
		right = other.vars.begin();
		
	while (true) {
		if (left == vars.end()) {
			if (right == vars.end())
				return true;
			return false;
		} else if (right == vars.end()) {
			return false;
        }
		
		if (*left->second->getType() != *right->second->getType())
			return false;
			
		++left;
		++right;
	}
	
	return true;
}

bool Struct::addVar(std::string name, Var *var) {
	std::string lower = boost::to_lower_copy(name);
	for (var_list::const_iterator iter = vars.begin(); iter != vars.end();
			++iter) {
	
		if (iter->first == lower)
			return false;
	}
	
	vars.push_back(std::make_pair(lower, var));
	return true;
}

bool PointerType::operator==(const Type &rhs) const {
	if (rhs.typeClass() != POINTER)
		return false;
	
	return *referenced == *static_cast<const PointerType &>(rhs).referenced;
}

namespace {
	std::string makeArrayName(const Type *ty, unsigned bounds) {
		std::stringstream ss;
		ss << ty->getName() << '[' << bounds << ']';
		return ss.str();
	}
	
	std::string makeArrayName(const Type *ty, std::vector<unsigned> bounds) {
		std::stringstream ss;
		ss << ty->getName() << '[';
		int size = bounds.size();
		for (int i = 0; i < size; ++i) {
			ss << bounds[i];
			if (i < size-1)
				ss << ',';
			else
				ss << ']';
		}
		return ss.str();
	}
}

ArrayType::ArrayType(const Type *ty, unsigned bounds) :
	Type(),
	referenced(ty),
	name(makeArrayName(ty, bounds)),
	cached(0),
	bounds(1, bounds),
	sd_bounds(bounds) { }

ArrayType::ArrayType(const Type *ty, std::vector<unsigned> bounds) :
		Type(),
		referenced(ty),
		name(makeArrayName(ty, bounds)),
		cached(0),
		bounds(bounds) {
			
	sd_bounds = bounds[0];
	for (unsigned i = 1; i < bounds.size(); ++i) {
		sd_bounds *= bounds[i];
	}
}

bool ArrayType::operator==(const Type &rhs) const {
	if (rhs.typeClass() != ARRAY)
		return false;
	
	return *referenced == *static_cast<const ArrayType &>(rhs).referenced;
}

bool SubType::operator==(const Type &rhs) const {
	if (rhs.typeClass() != SUBTYPE)
		return false;
	
	const SubType &other = static_cast<const SubType &>(rhs);
	if (*rtype != *other.rtype)
		return false;
		
	if (param_types.size() != other.param_types.size())
		return false;
		
	for (param_list::const_iterator it = param_types.begin(),
			it2 = other.param_types.begin();
			it != param_types.end();
			++it, ++it2) {
		
		if (**it != **it2)
			return false;
	}
	
	return true;
}

const std::string SubType::getName() const {
	/*if (!name.empty())
		return name;
	
	// Otherwise create the name.
	name = "{sub(";
	
	if (param_types.begin() != param_types.end()) {
		for (param_list::const_iterator it = param_types.begin();;) {
			name += (*it)->getName();
			
			++it;
			if (it == param_types.end())
				break;
				
			name += ",";
		}
	}
	
	if (!rtype->isVoid()) {
		name += "),";
		name += rtype->getName();
	} else {
		name += ")";
	}
	name += "}";
	*/
	return name;
}

