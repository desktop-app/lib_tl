# This file is part of Desktop App Toolkit,
# a set of libraries for developing nice desktop applications.
#
# For license and copyright information please follow this link:
# https://github.com/desktop-app/legal/blob/master/LEGAL
import glob, re, binascii, os, sys
from pprint import pprint

def readInputs(inputFiles):
  lines = []
  names = []
  layer = 0
  for inputFile in inputFiles:
    names.append(os.path.basename(inputFile))
    lines.append('---types---')
    with open(inputFile) as f:
      for line in f:
        layerline = re.match(r'// LAYER (\d+)', line)
        if (layerline):
          layer = int(layerline.group(1))
        else:
          lines.append(line)
  return lines, layer, names

# text serialization: types and funcs
def addTextSerialize(typeList, typeData, typesDict, idPrefix, primeType, boxed, prefix):
  result = ''
  for restype in typeList:
    v = typeData[restype]
    for data in v:
      name = data[0]
      prmsList = data[2]
      prms = data[3]
      hasFlags = data[4]
      conditionsList = data[5]
      conditions = data[6]
      trivialConditions = data[7]
      isTemplate = data[8]

      templateArgument = ''
      if (isTemplate != ''):
          templateArgument = '<SerializedRequest>'

      result += 'bool Serialize_' + name + '(DumpToTextBuffer &to, int32 stage, int32 lev, Types &types, Types &vtypes, StagesFlags &stages, StagesFlags &flags, const ' + primeType + ' *start, const ' + primeType + ' *end, uint32 iflag) {\n'
      if (len(conditions)):
        result += '\tauto flag = ' + prefix + name + templateArgument + '::Flags::from_raw(iflag);\n\n'
      if (len(prms)):
        result += '\tif (stage) {\n'
        result += '\t\tto.add(",\\n").addSpaces(lev);\n'
        result += '\t} else {\n'
        result += '\t\tto.add("{ ' + name + '");\n'
        result += '\t\tto.add("\\n").addSpaces(lev);\n'
        result += '\t}\n'
        result += '\tswitch (stage) {\n'
        stage = 0
        for k in prmsList:
          v = prms[k]
          result += '\tcase ' + str(stage) + ': to.add("  ' + k + ': "); ++stages.back(); '
          if (k == hasFlags):
            result += 'if (start >= end) return false; else flags.back() = *start; '
          if (k in trivialConditions):
            result += 'if (flag & ' + prefix + name + templateArgument + '::Flag::f_' + k + ') { '
            result += 'to.add("YES [ BY BIT ' + conditions[k] + ' IN FIELD ' + hasFlags + ' ]"); '
            result += '} else { to.add("[ SKIPPED BY BIT ' + conditions[k] + ' IN FIELD ' + hasFlags + ' ]"); } '
          else:
            if (k in conditions):
              result += 'if (flag & ' + prefix + name + templateArgument + '::Flag::f_' + k + ') { '
            result += 'types.push_back('
            vtypeget = re.match(r'^[Vv]ector<MTP([A-Za-z0-9\._]+)>', v)
            if (vtypeget):
              if (not re.match(r'^[A-Z]', v)):
                result += idPrefix + 'vector'
              else:
                result += '0'
              restype = vtypeget.group(1)
              try:
                if boxed[restype]:
                  restype = 0
              except KeyError:
                if re.match(r'^[A-Z]', restype):
                  restype = 0
            else:
              restype = v
              try:
                if boxed[restype]:
                  restype = 0
              except KeyError:
                if re.match(r'^[A-Z]', restype):
                  restype = 0
            if (restype):
              try:
                conses = typesDict[restype]
                if (len(conses) > 1):
                  print('Complex bare type found: "' + restype + '" trying to serialize "' + k + '" of type "' + v + '"')
                  continue
                if (vtypeget):
                  result += '); vtypes.push_back('
                result += idPrefix + conses[0][0]
                if (not vtypeget):
                  result += '); vtypes.push_back(0'
              except KeyError:
                if (vtypeget):
                  result += '); vtypes.push_back('
                if (re.match(r'^flags<', restype)):
                  result += idPrefix + 'flags'
                else:
                  result += idPrefix + restype + '+0'
                if (not vtypeget):
                  result += '); vtypes.push_back(0'
            else:
              if (not vtypeget):
                result += '0'
              result += '); vtypes.push_back(0'
            result += '); stages.push_back(0); flags.push_back(0); '
            if (k in conditions):
              result += '} else { to.add("[ SKIPPED BY BIT ' + conditions[k] + ' IN FIELD ' + hasFlags + ' ]"); } '
          result += 'break;\n'
          stage = stage + 1
        result += '\tdefault: to.add("}"); types.pop_back(); vtypes.pop_back(); stages.pop_back(); flags.pop_back(); break;\n'
        result += '\t}\n'
      else:
        result += '\tto.add("{ ' + name + ' }"); types.pop_back(); vtypes.pop_back(); stages.pop_back(); flags.pop_back();\n'
      result += '\treturn true;\n'
      result += '}\n\n'
  return result

# text serialization: types and funcs
def addTextSerializeInit(typeList, typeData, idPrefix):
  result = ''
  for restype in typeList:
    v = typeData[restype]
    for data in v:
      name = data[0]
      result += '\t\t{ ' + idPrefix + name + ', Serialize_' + name + ' },\n'
  return result

def generate(scheme):
  inputFiles = []
  outputPath = ''
  nextOutputPath = False
  for arg in sys.argv[1:]:
    if nextOutputPath:
      nextOutputPath = False
      outputPath = arg
    elif arg == '-o':
      nextOutputPath = True
    elif re.match(r'^-o(.+)', arg):
      outputPath = arg[2:]
    else:
      inputFiles.append(arg)

  if len(inputFiles) == 0:
    print('Input file required.')
    sys.exit(1)
  if outputPath == '':
    print('Output path required.')
    sys.exit(1)
  readAndGenerate(inputFiles, outputPath, scheme)

def readAndGenerate(inputFiles, outputPath, scheme):
  outputHeader = outputPath + '.h'
  outputSource = outputPath + '.cpp'
  outputConversionHeader = outputPath + '-conversion.h'
  outputConversionSource = outputPath + '-conversion.cpp'
  outputSerializationHeader = outputPath + '-dump_to_text.h'
  outputSerializationSource = outputPath + '-dump_to_text.cpp'
  outputHeaderBasename = os.path.basename(outputHeader)
  outputConversionHeaderBasename = os.path.basename(outputConversionHeader)
  outputSerializationHeaderBasename = os.path.basename(outputSerializationHeader)

  prefixes = scheme.get('prefixes', {})
  dataPrefix = prefixes.get('data', '')
  typePrefix = prefixes.get('type', '')
  idPrefix = prefixes.get('id') + '_'
  constructPrefix = prefixes.get('construct')
  def normalizedName(name):
    return name.replace('.', '_')
  def fullTypeName(name):
    return typePrefix + normalizedName(name)
  def fullDataName(name):
    return dataPrefix + normalizedName(name)
  def handleTemplate(name):
    templ = re.match(r'^([vV]ector<)([A-Za-z0-9\._<>]+)>$', name)
    if (templ):
      vectemplate = templ.group(2)
      if (vectemplate.find('<') >= 0):
        return templ.group(1) + fullTypeName(handleTemplate(vectemplate)) + '>'
      elif (re.match(r'^[A-Z]', vectemplate) or re.match(r'^[a-zA-Z0-9]+\.[A-Z]', vectemplate)):
        return templ.group(1) + fullTypeName(vectemplate) + '>'
      elif (vectemplate in builtinTypes):
        return templ.group(1) + fullTypeName(vectemplate) + '>'
      else:
        foundmeta = ''
        for metatype in typesDict:
          for typedata in typesDict[metatype]:
            if (typedata[0] == normalizedName(vectemplate)):
              foundmeta = metatype
              break
          if (len(foundmeta) > 0):
            break
        if (len(foundmeta) > 0):
          return templ.group(1) + fullTypeName(foundmeta) + '>'
        else:
          print('Bad vector param: ' + vectemplate)
          sys.exit(1)
    else:
      print('Bad template type: ' + name)
      sys.exit(1)

  namespaces = scheme.get('namespaces')
  globalNamespace = namespaces.get('global', '')
  creatorNamespace = namespaces.get('creator', '')
  creatorNamespaceFull = (globalNamespace + '::' if creatorNamespace != '' and globalNamespace != '' else globalNamespace) + creatorNamespace

  # this is a map (key flags -> map (flag name -> flag bit))
  # each key flag of parentFlags should be a subset of the value flag here
  parentFlagsCheck = {}
  flagInheritance = scheme.get('flagInheritance', {})
  typeIdExceptions = scheme.get('typeIdExceptions', [])
  renamedTypes = scheme.get('renamedTypes', {})
  skipLines = scheme.get('skip', [])
  builtinTypes = scheme.get('builtin', [])
  builtinTemplateTypes = scheme.get('builtinTemplates', [])
  builtinInclude = scheme.get('builtinInclude', '')
  nullableTypes = scheme.get('nullable', [])
  synonyms = scheme.get('synonyms', {})
  writeSections = scheme.get('sections', [])
  readWriteSection = 'read-write' in writeSections

  primitiveTypeNames = scheme.get('types')
  typeIdType = primitiveTypeNames.get('typeId')
  primeType = primitiveTypeNames.get('prime', '')
  bufferType = primitiveTypeNames.get('buffer', '')

  writeConversion = 'conversion' in scheme
  conversionScheme = scheme.get('conversion', {})
  conversionInclude = conversionScheme.get('include') if writeConversion else ''
  conversionNamespace = conversionScheme.get('namespace') if writeConversion else ''
  conversionBuiltinTypes = conversionScheme.get('builtinAdditional', [])
  conversionBuiltinInclude = conversionScheme.get('builtinInclude', '')
  def conversionName(name):
    return '::' + conversionNamespace + '::' + name
  def conversionTemplate(name, param):
    return conversionName(name) + '<' + conversionName(param) + '>'
  def conversionPointer(name):
    return conversionTemplate('object_ptr', name)
  def conversionMake(name):
    return conversionTemplate('make_object', name)
  def conversionMove(name):
    return conversionTemplate('move_object_as', name)

  writeSerialization = 'dumpToText' in scheme
  serializationScheme = scheme.get('dumpToText', {})
  serializationInclude = serializationScheme.get('include') if writeSerialization else ''

  if primeType == '' or bufferType == '':
    if readWriteSection or writeSerialization:
      print('Required types not provided.')
      sys.exit(1)

  def isBuiltinType(name):
    return name in builtinTypes or name in builtinTemplateTypes

  funcs = 0
  types = 0
  consts = 0
  funcsNow = 0
  enums = []
  funcsDict = {}
  funcsList = []
  typesDict = {}
  TypesDict = {}
  typesList = []
  TypeConstructors = {}
  boxed = {}
  funcsText = ''
  typesText = ''
  dataTexts = ''
  creatorProxyText = ''
  factories = ''
  flagOperators = ''
  methods = ''
  inlineMethods = ''
  visitorMethods = ''
  textSerializeInit = ''
  textSerializeMethods = ''
  forwards = ''
  forwTypedefs = ''
  conversionHeader = ''
  conversionSource = ''

  lines, layer, names = readInputs(inputFiles)
  inputNames = '\'' + '\', \''.join(names) + '\''

  for line in lines:
    nocomment = re.match(r'^(.*?)//', line)
    if (nocomment):
      line = nocomment.group(1)
    if (re.match(r'\-\-\-functions\-\-\-', line)):
      funcsNow = 1
      continue
    if (re.match(r'\-\-\-types\-\-\-', line)):
      funcsNow = 0
      continue
    if (re.match(r'^\s*$', line)):
      continue
    if line.strip() in skipLines:
      continue

    nametype = re.match(r'([a-zA-Z\.0-9_]+)(#[0-9a-f]+)?([^=]*)=\s*([a-zA-Z\.<>0-9_]+);', line)
    if (not nametype):
      print('Bad line found: ' + line)
      sys.exit(1)

    originalname = nametype.group(1)
    name = originalname
    if (name in renamedTypes):
      name = renamedTypes[name]
    nameInd = name.rfind('.')
    if (nameInd >= 0):
      Name = name[0:nameInd] + '_' + name[nameInd + 1:nameInd + 2].upper() + name[nameInd + 2:]
      name = normalizedName(name)
    else:
      Name = name[0:1].upper() + name[1:]
    typeid = nametype.group(2)
    if (typeid and len(typeid) > 0):
      typeid = typeid[1:]; # Skip '#'
    while (typeid and len(typeid) > 0 and typeid[0] == '0'):
      typeid = typeid[1:]

    cleanline = nametype.group(1) + nametype.group(3) + '= ' + nametype.group(4)
    cleanline = re.sub(r' [a-zA-Z0-9_]+\:flags\.[0-9]+\?true', '', cleanline)
    cleanline = cleanline.replace('<', ' ').replace('>', ' ').replace('  ', ' ')
    cleanline = re.sub(r'^ ', '', cleanline)
    cleanline = re.sub(r' $', '', cleanline)
    for synonym in synonyms:
        synonymOf = synonyms[synonym]
        cleanline = cleanline.replace(':' + synonym + ' ', ':' + synonymOf + ' ')
        cleanline = cleanline.replace('?' + synonym + ' ', '?' + synonymOf + ' ')
    cleanline = cleanline.replace('{', '')
    cleanline = cleanline.replace('}', '')
    countTypeId = binascii.crc32(binascii.a2b_qp(cleanline))
    if (countTypeId < 0):
      countTypeId += 2 ** 32
    countTypeId = re.sub(r'^0x|L$', '', hex(countTypeId))
    if (typeid and len(typeid) > 0):
      typeid = typeid
      if (typeid != countTypeId):
          key = originalname + '#' + typeid
          if (not key in typeIdExceptions):
            print('Warning: counted ' + countTypeId + ' mismatch with provided ' + typeid + ' (' + key + ', ' + cleanline + ')')
            continue
    else:
      typeid = countTypeId

    typeid = '0x' + typeid

    params = nametype.group(3)
    restype = nametype.group(4)
    if (restype.find('<') >= 0):
      restype = handleTemplate(restype)
    resType = normalizedName(restype)
    if (restype.find('.') >= 0):
      parts = re.match(r'([a-zA-Z0-9\.]+)\.([A-Z][A-Za-z0-9<>_]+)', restype)
      if (parts):
        restype = parts.group(1).replace('.', '_') + '_' + parts.group(2)[0:1].lower() + parts.group(2)[1:]
      else:
        print('Bad result type name with dot: ' + restype)
        sys.exit(1)
    else:
      if (re.match(r'^[A-Z]', restype)):
        restype = restype[:1].lower() + restype[1:]
      else:
        print('Bad result type name: ' + restype)
        sys.exit(1)

    boxed[resType] = restype
    boxed[Name] = name

    enums.append('\t' + idPrefix + name + ' = ' + typeid)

    paramsList = params.strip().split(' ')
    prms = {}
    conditions = {}
    trivialConditions = {}; # true type
    prmsList = []
    conditionsList = []
    isTemplate = hasFlags = hasTemplate = ''
    for param in paramsList:
      if (re.match(r'^\s*$', param)):
        continue
      templ = re.match(r'^{([A-Za-z]+):Type}$', param)
      if (templ):
        hasTemplate = templ.group(1)
        continue
      pnametype = re.match(r'([a-zA-Z_][a-zA-Z0-9_]*):([A-Za-z0-9<>\._]+|![a-zA-Z]+|\#|[a-z_][a-z0-9_]*\.[0-9]+\?[A-Za-z0-9<>\._]+)$', param)
      if (not pnametype):
        print('Bad param found: "' + param + '" in line: ' + line)
        sys.exit(1)
      pname = pnametype.group(1)
      ptypewide = pnametype.group(2)
      if (re.match(r'^!([a-zA-Z]+)$', ptypewide)):
        if ('!' + hasTemplate == ptypewide):
          isTemplate = pname
          ptype = 'TQueryType'
        else:
          print('Bad template param name: "' + param + '" in line: ' + line)
          sys.exit(1)
      elif (ptypewide == '#'):
        hasFlags = pname
        if funcsNow:
          ptype = 'flags<' + fullTypeName(name) + '::Flags>'
        else:
          ptype = 'flags<' + fullDataName(name) + '::Flags>'
      else:
        ptype = ptypewide
        if (ptype.find('?') >= 0):
          pmasktype = re.match(r'([a-z_][a-z0-9_]*)\.([0-9]+)\?([A-Za-z0-9<>\._]+)', ptype)
          if (not pmasktype or pmasktype.group(1) != hasFlags):
            print('Bad param found: "' + param + '" in line: ' + line)
            sys.exit(1)
          ptype = pmasktype.group(3)
          if (ptype.find('<') >= 0):
            ptype = handleTemplate(ptype)
          if (not pname in conditions):
            conditionsList.append(pname)
            conditions[pname] = pmasktype.group(2)
            if (ptype == 'true'):
              trivialConditions[pname] = 1
        elif (ptype.find('<') >= 0):
          ptype = handleTemplate(ptype)
      prmsList.append(pname)
      normalizedType = normalizedName(ptype)
      if (normalizedType in TypeConstructors):
        prms[pname] = TypeConstructors[normalizedType]['typeBare']
      else:
        prms[pname] = normalizedType

    if (isTemplate == '' and resType == 'X'):
      print('Bad response type "X" in "' + name +'" in line: ' + line)
      sys.exit(1)

    if funcsNow:
      methodBodies = ''
      if (isTemplate != ''):
        funcsText += '\ntemplate <typename TQueryType>'
      funcsText += '\nclass ' + fullTypeName(name) + ' { // RPC method \'' + nametype.group(1) + '\'\n'; # class

      funcsText += 'public:\n'

      prmsStr = []
      prmsInit = []
      prmsNames = []
      if (hasFlags != ''):
        funcsText += '\tenum class Flag : uint32 {\n'
        maxbit = 0
        parentFlagsCheck[fullTypeName(name)] = {}
        for paramName in conditionsList:
          funcsText += '\t\tf_' + paramName + ' = (1U << ' + conditions[paramName] + '),\n'
          parentFlagsCheck[fullTypeName(name)][paramName] = conditions[paramName]
          maxbit = max(maxbit, int(conditions[paramName]))
        if (maxbit > 0):
          funcsText += '\n'
        funcsText += '\t\tMAX_FIELD = (1U << ' + str(maxbit) + '),\n'
        funcsText += '\t};\n'
        funcsText += '\tusing Flags = base::flags<Flag>;\n'
        funcsText += '\tfriend inline constexpr bool is_flag_type(Flag) { return true; };\n'
        funcsText += '\n'

      if (len(prms) > len(trivialConditions)):
        for paramName in prmsList:
          if (paramName in trivialConditions):
            continue
          paramType = prms[paramName]
          prmsInit.append('_' + paramName + '(' + paramName + '_)')
          prmsNames.append(paramName + '_')
          if (paramName == isTemplate):
            ptypeFull = paramType
          else:
            ptypeFull = fullTypeName(paramType)
          if (paramType in ['int', 'Int', 'bool', 'Bool', 'flags<Flags>']):
            prmsStr.append(ptypeFull + ' ' + paramName + '_')
          else:
            prmsStr.append('const ' + ptypeFull + ' &' + paramName + '_')

      funcsText += '\t' + fullTypeName(name) + '();\n';# = default; # constructor
      if (isTemplate != ''):
        methodBodies += 'template <typename TQueryType>\n'
        methodBodies += fullTypeName(name) + '<TQueryType>::' + fullTypeName(name) + '() = default;\n'
      else:
        methodBodies += fullTypeName(name) + '::' + fullTypeName(name) + '() = default;\n'
      if (len(prms) > len(trivialConditions)):
        explicitText = 'explicit ' if (len(prms) - len(trivialConditions) == 1) else ''
        funcsText += '\t' + explicitText + fullTypeName(name) + '(' + ', '.join(prmsStr) + ');\n'
        if (isTemplate != ''):
          methodBodies += 'template <typename TQueryType>\n'
          methodBodies += fullTypeName(name) + '<TQueryType>::' + fullTypeName(name) + '(' + ', '.join(prmsStr) + ') : ' + ', '.join(prmsInit) + ' {\n}\n'
        else:
          methodBodies += fullTypeName(name) + '::' + fullTypeName(name) + '(' + ', '.join(prmsStr) + ') : ' + ', '.join(prmsInit) + ' {\n}\n'

      funcsText += '\t' + typeIdType + ' type() const {\n\t\treturn ' + idPrefix + name + ';\n\t}\n'; # type id
      if readWriteSection:
        funcsText += '\n'
        funcsText += '\ttemplate <typename Prime>\n'
        funcsText += '\t[[nodiscard]] bool read(const Prime *&from, const Prime *end, ' + typeIdType + ' cons = ' + idPrefix + name + ');\n'; # read method
        if (isTemplate != ''):
          methodBodies += 'template <typename TQueryType>\n'
          methodBodies += 'template <typename Prime>\n'
          methodBodies += 'bool ' + fullTypeName(name) + '<TQueryType>::read(const Prime *&from, const Prime *end, ' + typeIdType + ' cons) {\n'
        else:
          methodBodies += 'template <typename Prime>\n'
          methodBodies += 'bool ' + fullTypeName(name) + '::read(const Prime *&from, const Prime *end, ' + typeIdType + ' cons) {\n'
        readFunc = ''
        for k in prmsList:
          v = prms[k]
          if (k in conditionsList):
            if (not k in trivialConditions):
              readFunc += '\t\t&& ((_' + hasFlags + '.v & Flag::f_' + k + ') ? _' + k + '.read(from, end) : ((_' + k + ' = ' + fullTypeName(v) + '()), true))\n'
          else:
            readFunc += '\t\t&& _' + k + '.read(from, end)\n'
        if readFunc != '':
          methodBodies += '\treturn' + readFunc[4:len(readFunc)-1] + ';\n'
        else:
          methodBodies += '\treturn true;\n'
        methodBodies += '}\n'
        if isTemplate == '':
          methodBodies += 'template bool ' + fullTypeName(name) + '::read<' + primeType + '>(const ' + primeType + ' *&from, const ' + primeType + ' *end, ' + typeIdType + ' cons);\n'

        funcsText += '\ttemplate <typename Accumulator>\n'
        funcsText += '\tvoid write(Accumulator &to) const;\n'; # write method
        if (isTemplate != ''):
          methodBodies += 'template <typename TQueryType>\n'
          methodBodies += 'template <typename Accumulator>\n'
          methodBodies += 'void ' + fullTypeName(name) + '<TQueryType>::write(Accumulator &to) const {\n'
        else:
          methodBodies += 'template <typename Accumulator>\n'
          methodBodies += 'void ' + fullTypeName(name) + '::write(Accumulator &to) const {\n'
        for k in prmsList:
          v = prms[k]
          if (k in conditionsList):
            if (not k in trivialConditions):
              methodBodies += '\tif (_' + hasFlags + '.v & Flag::f_' + k + ') _' + k + '.write(to);\n'
          else:
            methodBodies += '\t_' + k + '.write(to);\n'
        methodBodies += '}\n'
        if isTemplate == '':
          methodBodies += 'template void ' + fullTypeName(name) + '::write<' + bufferType + '>(' + bufferType + ' &to) const;\n'
          methodBodies += 'template void ' + fullTypeName(name) + '::write<::tl::details::LengthCounter>(::tl::details::LengthCounter &to) const;\n'

      if writeConversion:
        conversionHeader += fullTypeName(name) + ' tl_from(' + conversionPointer(name) + ' &&value);\n'
        conversionHeader += conversionPointer(name) + ' tl_to(const ' + fullTypeName(name) + ' &value);\n'
        conversionSource += '\n\
' + fullTypeName(name) + ' tl_from(' + conversionPointer(name) + ' &&value) {\n\
\treturn ' + fullTypeName(name) + '('
        conversionArguments = []
        for k in prmsList:
          if k in conditionsList:
            print('Conversion with flags :(')
            sys.exit(1)
          else:
            conversionArguments.append('tl_from(std::move(value->' + k + '_))')
        conversionSource += ', '.join(conversionArguments) + ');\n\
}\n\
\n\
' + conversionPointer(name) + ' tl_to(const '+  fullTypeName(name) + ' &value) {\n\
\treturn ' + conversionMake(name) + '('
        conversionArguments = []
        for k in prmsList:
          if (k in conditionsList):
            print('Conversion with flags :(')
            sys.exit(1)
          else:
            conversionArguments.append('tl_to(value.v' + k + '())')
        conversionSource += ', '.join(conversionArguments) + ');\n\
}\n'
        if len(prmsList) > 0:
          funcsText += '\n'
          for paramName in prmsList: # getters
            if (paramName in trivialConditions):
              continue
            paramType = prms[paramName]
            if (paramName in conditions):
              funcsText += '\t[[nodiscard]] tl::conditional<' + fullTypeName(paramType) + '> v' + paramName + '() const;\n'
              methodBodies += 'tl::conditional<' + fullTypeName(paramType) + '> ' + fullTypeName(name) + '::v' + paramName + '() const {\n'
              methodBodies += '\treturn (_' + hasFlags + '.v & Flag::f_' + paramName + ') ? &_' + paramName + ' : nullptr;\n'
              methodBodies += '}\n'
            else:
              funcsText += '\t[[nodiscard]] const ' + fullTypeName(paramType) + ' &v' + paramName + '() const;\n'
              methodBodies += 'const ' + fullTypeName(paramType) + ' &' + fullTypeName(name) + '::v' + paramName + '() const {\n'
              methodBodies += '\treturn _' + paramName + ';\n'
              methodBodies += '}\n'

      if (isTemplate != ''):
        funcsText += '\n\tusing ResponseType = typename TQueryType::ResponseType;\n\n'
        inlineMethods += methodBodies
      else:
        funcsText += '\n\tusing ResponseType = ' + fullTypeName(resType) + ';\n\n'; # method return type
        methods += methodBodies

      if (len(prms) > len(trivialConditions)):
        funcsText += 'private:\n'
        for paramName in prmsList:
          if (paramName in trivialConditions):
            continue
          paramType = prms[paramName]
          if (paramName == isTemplate):
            ptypeFull = paramType
          else:
            ptypeFull = fullTypeName(paramType)
          funcsText += '\t' + ptypeFull + ' _' + paramName + ';\n'
        funcsText += '\n'

      funcsText += '};\n'; # class ending
      if (isTemplate != ''):
        funcsText += 'template <typename TQueryType>\n'
        funcsText += 'using ' + fullTypeName(Name) + ' = tl::boxed<' + fullTypeName(name) + '<TQueryType>>;\n'
      else:
        funcsText += 'using ' + fullTypeName(Name) + ' = tl::boxed<' + fullTypeName(name) + '>;\n'
      funcs = funcs + 1

      if (not restype in funcsDict):
        funcsList.append(restype)
        funcsDict[restype] = []
#        TypesDict[restype] = resType
      funcsDict[restype].append([name, typeid, prmsList, prms, hasFlags, conditionsList, conditions, trivialConditions, isTemplate])
    else:
      if (isTemplate != ''):
        print('Template types not allowed: "' + resType + '" in line: ' + line)
        continue
      if (not restype in typesDict):
        typesList.append(restype)
        typesDict[restype] = []
      TypesDict[restype] = resType
      typesDict[restype].append([name, typeid, prmsList, prms, hasFlags, conditionsList, conditions, trivialConditions, isTemplate])

      TypeConstructors[name] = {'typeBare': restype, 'typeBoxed': resType}

      consts = consts + 1

  for typeName in builtinTypes:
    forwTypedefs += 'using ' + fullTypeName(typeName[:1].upper() + typeName[1:]) + ' = tl::boxed<' + fullTypeName(typeName) + '>;\n'
  for typeName in builtinTemplateTypes:
    forwTypedefs += 'template <typename T>\n'
    forwTypedefs += 'using ' + fullTypeName(typeName[:1].upper() + typeName[1:]) + ' = tl::boxed<' + fullTypeName(typeName) + '<T>>;\n'

  textSerializeMethods += addTextSerialize(typesList, typesDict, typesDict, idPrefix, primeType, boxed, dataPrefix)
  textSerializeInit += addTextSerializeInit(typesList, typesDict, idPrefix) + '\n'
  textSerializeMethods += addTextSerialize(funcsList, funcsDict, typesDict, idPrefix, primeType, boxed, typePrefix)
  textSerializeInit += addTextSerializeInit(funcsList, funcsDict, idPrefix) + '\n'

  for restype in typesList:
    v = typesDict[restype]
    resType = TypesDict[restype]
    withData = 0
    creatorsDeclarations = ''
    creatorsBodies = ''
    flagDeclarations = ''
    constructsText = ''
    constructsBodies = ''

    forwards += 'class ' + fullTypeName(restype) + ';\n'
    forwTypedefs += 'using ' + fullTypeName(resType) + ' = tl::boxed<' + fullTypeName(restype) + '>;\n'

    withType = (len(v) > 1)
    nullable = restype in nullableTypes
    switchLines = ''
    friendDecl = ''
    getters = ''
    visitor = ''
    reader = ''
    writer = ''
    newFast = ''

    if writeConversion:
      if not restype in builtinTypes and not restype in conversionBuiltinTypes:
        if len(v) > 1:
          conversionType = resType
          conversionHeader += fullTypeName(resType) + ' tl_from(' + conversionPointer(conversionType) + ' &&value);\n'
          conversionSource += '\n' + fullTypeName(resType) + ' tl_from(' + conversionPointer(conversionType) + ' &&value) {\n'
          if nullable:
            conversionSource += '\tif (!value) {\n\t\treturn nullptr;\n\t}\n\n'
          else:
            conversionSource += '\tExpects(value != nullptr);\n\n'
          conversionSource += '\tswitch (' + typeIdType + '(value->get_id())) {\n'
          for data in v:
            name = data[0]
            prms = data[3]
            trivialConditions = data[7]
            conversionSource += '\tcase uint32(' + conversionName(name) + '::ID): return tl_from(' + conversionMove(name) + '(std::move(value)));\n'
          conversionSource += '\tdefault: Unexpected("Type in ' + fullTypeName(restype) + ' tl_from.");\n\t}\n}\n'
        else:
          conversionType = v[0][0]

        conversionHeader += conversionPointer(conversionType) + ' tl_to(const ' + fullTypeName(restype) + ' &value);\n'
        conversionSource += '\n' + conversionPointer(conversionType) + ' tl_to(const ' + fullTypeName(restype) + ' &value) {\n'
        if withType:
          conversionSource += '\tswitch (value.type()) {\n'
          if nullable:
            conversionSource += '\tcase ' + typeIdType + '(0): return nullptr;\n'
          for data in v:
            name = data[0]
            prms = data[3]
            trivialConditions = data[7]
            if (len(prms) == len(trivialConditions)):
              conversionSource += '\tcase ' + idPrefix + name + ': return ' + conversionMake(name) + '();\n'
            else:
              conversionSource += '\tcase ' + idPrefix + name + ': return tl_to(value.c_' + name + '());\n'
          conversionSource += '\tdefault: Unexpected("Type in tl_to(' + fullTypeName(restype) + ').");\n\t}\n'
        else:
          if nullable:
            conversionSource += '\tif (!value) {\n\t\treturn nullptr;\n\t}\n'
          conversionSource += '\treturn tl_to(value.c_' + v[0][0] + '());\n'
        conversionSource += '}\n'

    for data in v:
      name = data[0]
      typeid = data[1]
      prmsList = data[2]
      prms = data[3]
      hasFlags = data[4]
      conditionsList = data[5]
      conditions = data[6]
      trivialConditions = data[7]

      dataText = ''
      if (len(prms) > len(trivialConditions)):
        withData = 1
        dataText += '\nclass ' + fullDataName(name) + ' : public tl::details::type_data {\n'; # data class
      else:
        dataText += '\nclass ' + fullDataName(name) + ' {\n'; # empty data class for visitors
      dataText += 'public:\n'
      dataText += '\ttemplate <typename Other>\n'
      dataText += '\tstatic constexpr bool Is() { return std::is_same_v<std::decay_t<Other>, ' + fullDataName(name) + '>; };\n\n'
      creatorParams = []
      creatorParamsList = []
      readText = ''
      writeText = ''

      if (hasFlags != ''):
        dataText += '\tenum class Flag : uint32 {\n'
        maxbit = 0
        parentFlagsCheck[fullDataName(name)] = {}
        for paramName in conditionsList:
          dataText += '\t\tf_' + paramName + ' = (1U << ' + conditions[paramName] + '),\n'
          parentFlagsCheck[fullDataName(name)][paramName] = conditions[paramName]
          maxbit = max(maxbit, int(conditions[paramName]))
        if (maxbit > 0):
          dataText += '\n'
        dataText += '\t\tMAX_FIELD = (1U << ' + str(maxbit) + '),\n'
        dataText += '\t};\n'
        dataText += '\tusing Flags = base::flags<Flag>;\n'
        dataText += '\tfriend inline constexpr bool is_flag_type(Flag) { return true; };\n'
        dataText += '\n'
        if (len(conditions)):
          for paramName in conditionsList:
            if (paramName in trivialConditions):
              dataText += '\t[[nodiscard]] bool is_' + paramName + '() const;\n'
              constructsBodies += 'bool ' + fullDataName(name) + '::is_' + paramName + '() const {\n'
              constructsBodies += '\treturn _' + hasFlags + '.v & Flag::f_' + paramName + ';\n'
              constructsBodies += '}\n'
          dataText += '\n'

      switchLines += '\tcase ' + idPrefix + name + ': '; # for by-type-id type constructor
      getters += '\t[[nodiscard]] const ' + fullDataName(name) + ' &c_' + name + '() const;\n'; # const getter
      visitor += '\tcase ' + idPrefix + name + ': return base::match_method(c_' + name + '(), std::forward<Method>(method), std::forward<Methods>(methods)...);\n'

      forwards += 'class ' + fullDataName(name) + ';\n'; # data class forward declaration
      if (len(prms) > len(trivialConditions)):
        dataText += '\t' + fullDataName(name) + '();\n'; # default constructor
        switchLines += 'setData(new ' + fullDataName(name) + '()); '

        constructsBodies += fullDataName(name) + '::' + fullDataName(name) + '() = default;\n'
        constructsBodies += 'const ' + fullDataName(name) + ' &' + fullTypeName(restype) + '::c_' + name + '() const {\n'
        if (withType):
          constructsBodies += '\tExpects(_type == ' + idPrefix + name + ');\n\n'
        constructsBodies += '\treturn queryData<' + fullDataName(name) + '>();\n'
        constructsBodies += '}\n'

        constructsText += '\texplicit ' + fullTypeName(restype) + '(const ' + fullDataName(name) + ' *data);\n'; # by-data type constructor
        constructsBodies += fullTypeName(restype) + '::' + fullTypeName(restype) + '(const ' + fullDataName(name) + ' *data) : type_owner(data)'
        if (withType):
          constructsBodies += ', _type(' + idPrefix + name + ')'
        constructsBodies += ' {\n}\n'

        dataText += '\t' + fullDataName(name) + '('; # params constructor
        prmsStr = []
        prmsInit = []
        for paramName in prmsList:
          if (paramName in trivialConditions):
            continue
          paramType = prms[paramName]

          if (paramType in ['int', 'Int', 'bool', 'Bool']):
            prmsStr.append(fullTypeName(paramType) + ' ' + paramName + '_')
            creatorParams.append(fullTypeName(paramType) + ' ' + paramName + '_')
          else:
            prmsStr.append('const ' + fullTypeName(paramType) + ' &' + paramName + '_')
            creatorParams.append('const ' + fullTypeName(paramType) + ' &' + paramName + '_')
          creatorParamsList.append(paramName + '_')
          prmsInit.append('_' + paramName + '(' + paramName + '_)')
          if withType:
            writeText += '\t'
          if (paramName in conditions):
            readText += '\t\t&& (v' + paramName + '() ? _' + paramName + '.read(from, end) : ((_' + paramName + ' = ' + fullTypeName(paramType) + '()), true))\n'
            writeText += '\tif (const auto v' + paramName + ' = v.v' + paramName + '()) v' + paramName + '->write(to);\n'
          else:
            readText += '\t\t&& _' + paramName + '.read(from, end)\n'
            writeText += '\tv.v' + paramName + '().write(to);\n'

        dataText += ', '.join(prmsStr) + ');\n'

        constructsBodies += fullDataName(name) + '::' + fullDataName(name) + '(' + ', '.join(prmsStr) + ') : ' + ', '.join(prmsInit) + ' {\n}\n'

        if readWriteSection:
          dataText += '\n'
          dataText += '\t[[nodiscard]] bool read(const ' + primeType + ' *&from, const ' + primeType + ' *end);\n'

          constructsBodies += 'bool ' + fullDataName(name) + '::read(const ' + primeType + ' *&from, const ' + primeType + ' *end) {\n'
          if readText != '':
            constructsBodies += '\treturn' + readText[4:len(readText)-1] + ';\n'
          else:
            constructsBodies += '\treturn true;\n'
          constructsBodies += '}\n'

        dataText += '\n'
        if len(prmsList) > 0:
          for paramName in prmsList: # getters
            if (paramName in trivialConditions):
              continue
            paramType = prms[paramName]
            if (paramName in conditions):
              dataText += '\t[[nodiscard]] tl::conditional<' + fullTypeName(paramType) + '> v' + paramName + '() const;\n'
              constructsBodies += 'tl::conditional<' + fullTypeName(paramType) + '> ' + fullDataName(name) + '::v' + paramName + '() const {\n'
              constructsBodies += '\treturn (_' + hasFlags + '.v & Flag::f_' + paramName + ') ? &_' + paramName + ' : nullptr;\n'
              constructsBodies += '}\n'
            else:
              dataText += '\t[[nodiscard]] const ' + fullTypeName(paramType) + ' &v' + paramName + '() const;\n'
              constructsBodies += 'const ' + fullTypeName(paramType) + ' &' + fullDataName(name) + '::v' + paramName + '() const {\n'
              constructsBodies += '\treturn _' + paramName + ';\n'
              constructsBodies += '}\n'
          dataText += '\n'
          dataText += 'private:\n'
          for paramName in prmsList: # fields declaration
            if (paramName in trivialConditions):
              continue
            paramType = prms[paramName]
            dataText += '\t' + fullTypeName(paramType) + ' _' + paramName + ';\n'
          dataText += '\n'
        newFast = 'new ' + fullDataName(name) + '()'
      else:
        constructsBodies += 'const ' + fullDataName(name) + ' &' + fullTypeName(restype) + '::c_' + name + '() const {\n'
        if (withType):
          constructsBodies += '\tExpects(_type == ' + idPrefix + name + ');\n\n'
        constructsBodies += '\tstatic const ' + fullDataName(name) + ' result;\n'
        constructsBodies += '\treturn result;\n'
        constructsBodies += '}\n'

      if writeConversion and not restype in builtinTypes and not restype in conversionBuiltinTypes:
        conversionHeader += fullTypeName(restype) + ' tl_from(' + conversionPointer(name) + ' &&value);\n'
        conversionHeader += conversionPointer(name) + ' tl_to(const ' + fullDataName(name) + ' &value);\n'
        conversionSource += '\n' + fullTypeName(restype) + ' tl_from(' + conversionPointer(name) + ' &&value) {\n'
        if nullable:
          conversionSource += '\tif (!value) {\n\t\treturn nullptr;\n\t}\n\n'
        conversionSource += '\treturn ' + constructPrefix + normalizedName(name) + '('
        conversionArguments = []
        for k in prmsList:
          if k in conditionsList:
            print('Conversion with flags :(')
            sys.exit(1)
          else:
            conversionArguments.append('tl_from(std::move(value->' + k + '_))')
        conversionSource += ', '.join(conversionArguments) + ');\n'
        conversionSource += '}\n\n' + conversionPointer(name) + ' tl_to(const ' + fullDataName(name) + ' &value) {\n'
        conversionSource += '\treturn ' + conversionMake(name) + '('
        conversionArguments = []
        for k in prmsList:
          if (k in conditionsList):
            print('Conversion with flags :(')
            sys.exit(1)
          else:
            conversionArguments.append('tl_to(value.v' + k + '())')
        conversionSource += ', '.join(conversionArguments) + ');\n}\n'

      switchLines += 'break;\n'
      dataText += '};\n'; # class ending

      dataTexts += dataText; # add data class

      if not friendDecl:
        friendDecl += '\tfriend class ::' + creatorNamespaceFull + '::TypeCreator;\n'
      creatorProxyText += '\tinline static ' + fullTypeName(restype) + ' new_' + name + '(' + ', '.join(creatorParams) + ') {\n'
      if len(prms) > len(trivialConditions): # creator with params
        creatorProxyText += '\t\treturn ' + fullTypeName(restype) + '(new ' + fullDataName(name) + '(' + ', '.join(creatorParamsList) + '));\n'
      else:
        if withType: # creator by type
          creatorProxyText += '\t\treturn ' + fullTypeName(restype) + '(' + idPrefix + name + ');\n'
        else: # single creator
          creatorProxyText += '\t\treturn ' + fullTypeName(restype) + '();\n'
      creatorProxyText += '\t}\n'
      creatorsDeclarations += fullTypeName(restype) + ' ' + constructPrefix + name + '(' + ', '.join(creatorParams) + ');\n'
      creatorsBodies += fullTypeName(restype) + ' ' + constructPrefix + name + '(' + ', '.join(creatorParams) + ') {\n'
      creatorsBodies += '\treturn ::' + creatorNamespaceFull + '::TypeCreator::new_' + name + '(' + ', '.join(creatorParamsList) + ');\n'
      creatorsBodies += '}\n'

      if (withType):
        reader += '\tcase ' + idPrefix + name + ': _type = cons; '; # read switch line
        if (len(prms) > len(trivialConditions)):
          reader += '{\n'
          reader += '\t\tif (const auto data = new ' + fullDataName(name) + '(); data->read(from, end)) {\n'
          reader += '\t\t\tsetData(data);\n'
          reader += '\t\t} else {\n'
          reader += '\t\t\tdelete data;\n'
          reader += '\t\t\treturn false;\n'
          reader += '\t\t}\n'
          reader += '\t} break;\n'

          writer += '\tcase ' + idPrefix + name + ': {\n'; # write switch line
          writer += '\t\tconst ' + fullDataName(name) + ' &v = c_' + name + '();\n'
          writer += writeText
          writer += '\t} break;\n'
        else:
          reader += 'break;\n'
      else:
        if (len(prms) > len(trivialConditions)):
          reader += '\tif (const auto data = new ' + fullDataName(name) + '(); data->read(from, end)) {\n'
          reader += '\t\tsetData(data);\n'
          reader += '\t} else {\n'
          reader += '\t\tdelete data;\n'
          reader += '\t\treturn false;\n'
          reader += '\t}\n'

          writer += '\tconst ' + fullDataName(name) + ' &v = c_' + name + '();\n'
          writer += writeText

    if nullable:
      if not withType and not withData:
        print('No way to make a nullable non-data-owner non-type-distinct type')
        sys.exit(1)
      elif readWriteSection:
        print('No way to make read-write code for a nullable type')
        sys.exit(1)

    forwards += '\n'

    typesText += '\nclass ' + fullTypeName(restype); # type class declaration
    if withData:
      typesText += ' : private tl::details::type_owner'; # if has data fields
    typesText += ' {\n'
    typesText += 'public:\n'
    typesText += '\t' + fullTypeName(restype) + '();\n'; # default constructor
    if withData and not withType:
      methods += '\n' + fullTypeName(restype) + '::' + fullTypeName(restype) + '() : type_owner(' + newFast + ') {\n}\n'
    else:
      methods += '\n' + fullTypeName(restype) + '::' + fullTypeName(restype) + '() = default;\n'

    if nullable:
      typesText += '\t' + fullTypeName(restype) + '(std::nullptr_t);\n'
      methods += fullTypeName(restype) + '::' + fullTypeName(restype) + '(std::nullptr_t) {\n}\n'
    typesText += '\n'
    if nullable:
      typesText += '\texplicit operator bool() const;\n'
      methods += fullTypeName(restype) + '::operator bool() const {\n\t'
      if withData:
        methods += '\treturn hasData();\n'
      else:
        methods += '\treturn _type != 0;\n'
      methods += '}\n'
    typesText += getters
    typesText += '\n'
    typesText += '\ttemplate <typename Method, typename ...Methods>\n'
    typesText += '\tdecltype(auto) match(Method &&method, Methods &&...methods) const;\n'
    visitorMethods += 'template <typename Method, typename ...Methods>\n'
    visitorMethods += 'decltype(auto) ' + fullTypeName(restype) + '::match(Method &&method, Methods &&...methods) const {\n'
    if withType:
      visitorMethods += '\tswitch (_type) {\n'
      visitorMethods += visitor
      visitorMethods += '\t}\n'
      visitorMethods += '\tUnexpected("Type in ' + fullTypeName(restype) + '::match.");\n'
    else:
      visitorMethods += '\treturn base::match_method(c_' + v[0][0] + '(), std::forward<Method>(method), std::forward<Methods>(methods)...);\n'
    visitorMethods += '}\n\n'

    typesText += '\t' + typeIdType + ' type() const;\n'; # type id method
    methods += typeIdType + ' ' + fullTypeName(restype) + '::type() const {\n'
    if withType:
      if nullable:
        methods += '\treturn _type;\n'
      else:
        methods += '\tExpects(_type != 0);\n\n'
        methods += '\treturn _type;\n'
    else:
      if nullable:
        methods += '\treturn hasData() ? ' + idPrefix + v[0][0] + ' : ' + typeIdType + '(0);\n'
      else:
        methods += '\treturn ' + idPrefix + v[0][0] + ';\n'
    methods += '}\n'

    if readWriteSection:
      typesText += '\n'
      typesText += '\t[[nodiscard]] bool read(const ' + primeType + ' *&from, const ' + primeType + ' *end, ' + typeIdType + ' cons'; # read method
      if (not withType):
        typesText += ' = ' + idPrefix + name
      typesText += ');\n'
      methods += 'bool ' + fullTypeName(restype) + '::read(const ' + primeType + ' *&from, const ' + primeType + ' *end, ' + typeIdType + ' cons) {\n'
      if (withData):
        if not (withType):
          methods += '\tif (cons != ' + idPrefix + v[0][0] + ') return false;\n'
      if (withType):
        methods += '\tswitch (cons) {\n'
        methods += reader
        methods += '\tdefault: return false;\n'
        methods += '\t}\n'
      else:
        methods += reader
      methods += '\treturn true;\n'
      methods += '}\n'

      typesText += '\ttemplate <typename Accumulator>\n' # write method
      typesText += '\tvoid write(Accumulator &to) const;\n'
      methods += 'template <typename Accumulator>\n'
      methods += 'void ' + fullTypeName(restype) + '::write(Accumulator &to) const {\n'
      if (withType and writer != ''):
        methods += '\tswitch (_type) {\n'
        methods += writer
        methods += '\t}\n'
      else:
        methods += writer
      methods += '}\n'
      methods += 'template void ' + fullTypeName(restype) + '::write<' + bufferType + '>(' + bufferType + ' &to) const;\n'
      methods += 'template void ' + fullTypeName(restype) + '::write<::tl::details::LengthCounter>(::tl::details::LengthCounter &to) const;\n'

    typesText += '\n\tusing ResponseType = void;\n'; # no response types declared

    typesText += '\nprivate:\n'; # private constructors
    if (withType): # by-type-id constructor
      typesText += '\texplicit ' + fullTypeName(restype) + '(' + typeIdType + ' type);\n'
      methods += fullTypeName(restype) + '::' + fullTypeName(restype) + '(' + typeIdType + ' type) : '
      methods += '_type(type)'
      methods += ' {\n'
      methods += '\tswitch (type) {\n'; # type id check
      methods += switchLines
      methods += '\tdefault: Unexpected("Type in ' + fullTypeName(restype) + '::' + fullTypeName(restype) + '.");\n'
      methods += '\t}\n'
      methods += '}\n'; # by-type-id constructor end

    if (withData):
      typesText += constructsText
    methods += constructsBodies

    if (friendDecl):
      typesText += '\n' + friendDecl

    if (withType):
      typesText += '\n\t' + typeIdType + ' _type = 0;\n'; # type field var

    typesText += '};\n'; # type class ended

    flagOperators += flagDeclarations
    factories += creatorsDeclarations
    methods += creatorsBodies
    typesText += 'using ' + fullTypeName(resType) + ' = tl::boxed<' + fullTypeName(restype) + '>;\n'; # boxed type definition

  flagOperators += '\n'

  for pureChildName in flagInheritance:
    childName = dataPrefix + pureChildName
    parentName = dataPrefix + flagInheritance[pureChildName]
    for flag in parentFlagsCheck[childName]:
  #
  # 'channelForbidden' has 'until_date' flag and 'channel' doesn't have it.
  # But as long as flags don't collide this is not a problem.
  #
  #    if (not flag in parentFlagsCheck[parentName]):
  #      print('Flag ' + flag + ' not found in ' + parentName + ' which should be a flags-parent of ' + childName)
  #      sys.exit(1)
  #
      if (flag in parentFlagsCheck[parentName]):
        if (parentFlagsCheck[childName][flag] != parentFlagsCheck[parentName][flag]):
          print('Flag ' + flag + ' has different value in ' + parentName + ' which should be a flags-parent of ' + childName)
          sys.exit(1)
      else:
        parentFlagsCheck[parentName][flag] = parentFlagsCheck[childName][flag]
    flagOperators += 'inline ' + parentName + '::Flags mtpCastFlags(' + childName + '::Flags flags) { return static_cast<' + parentName + '::Flag>(flags.value()); }\n'
    flagOperators += 'inline ' + parentName + '::Flags mtpCastFlags(MTPflags<' + childName + '::Flags> flags) { return mtpCastFlags(flags.v); }\n'

  textSerializeSource = ''
  if writeSerialization:
    # manual types added here

    textSerializeMethods += '\
bool Serialize_rpc_result(DumpToTextBuffer &to, int32 stage, int32 lev, Types &types, Types &vtypes, StagesFlags &stages, StagesFlags &flags, const ' + primeType + ' *start, const ' + primeType + ' *end, uint32 iflag) {\n\
	if (stage) {\n\
		to.add(",\\n").addSpaces(lev);\n\
	} else {\n\
		to.add("{ rpc_result");\n\
		to.add("\\n").addSpaces(lev);\n\
	}\n\
	switch (stage) {\n\
	case 0: to.add("  req_msg_id: "); ++stages.back(); types.push_back(' + idPrefix + 'long); vtypes.push_back(0); stages.push_back(0); flags.push_back(0); break;\n\
	case 1: to.add("  result: "); ++stages.back(); types.push_back(0); vtypes.push_back(0); stages.push_back(0); flags.push_back(0); break;\n\
	default: to.add("}"); types.pop_back(); vtypes.pop_back(); stages.pop_back(); flags.pop_back(); break;\n\
	}\n\
	return true;\n\
}\n\
\n\
bool Serialize_msg_container(DumpToTextBuffer &to, int32 stage, int32 lev, Types &types, Types &vtypes, StagesFlags &stages, StagesFlags &flags, const ' + primeType + ' *start, const ' + primeType + ' *end, uint32 iflag) {\n\
	if (stage) {\n\
		to.add(",\\n").addSpaces(lev);\n\
	} else {\n\
		to.add("{ msg_container");\n\
		to.add("\\n").addSpaces(lev);\n\
	}\n\
	switch (stage) {\n\
	case 0: to.add("  messages: "); ++stages.back(); types.push_back(' + idPrefix + 'vector); vtypes.push_back(' + idPrefix + 'core_message); stages.push_back(0); flags.push_back(0); break;\n\
	default: to.add("}"); types.pop_back(); vtypes.pop_back(); stages.pop_back(); flags.pop_back(); break;\n\
	}\n\
	return true;\n\
}\n\
\n\
bool Serialize_core_message(DumpToTextBuffer &to, int32 stage, int32 lev, Types &types, Types &vtypes, StagesFlags &stages, StagesFlags &flags, const ' + primeType + ' *start, const ' + primeType + ' *end, uint32 iflag) {\n\
	if (stage) {\n\
		to.add(",\\n").addSpaces(lev);\n\
	} else {\n\
		to.add("{ core_message");\n\
		to.add("\\n").addSpaces(lev);\n\
	}\n\
	switch (stage) {\n\
	case 0: to.add("  msg_id: "); ++stages.back(); types.push_back(' + idPrefix + 'long); vtypes.push_back(0); stages.push_back(0); flags.push_back(0); break;\n\
	case 1: to.add("  seq_no: "); ++stages.back(); types.push_back(' + idPrefix + 'int); vtypes.push_back(0); stages.push_back(0); flags.push_back(0); break;\n\
	case 2: to.add("  bytes: "); ++stages.back(); types.push_back(' + idPrefix + 'int); vtypes.push_back(0); stages.push_back(0); flags.push_back(0); break;\n\
	case 3: to.add("  body: "); ++stages.back(); types.push_back(0); vtypes.push_back(0); stages.push_back(0); flags.push_back(0); break;\n\
	default: to.add("}"); types.pop_back(); vtypes.pop_back(); stages.pop_back(); flags.pop_back(); break;\n\
	}\n\
	return true;\n\
}\n\
\n'

    textSerializeInit += '\
	    { ' + idPrefix + 'rpc_result, Serialize_rpc_result },\n\
	    { ' + idPrefix + 'msg_container, Serialize_msg_container },\n\
	    { ' + idPrefix + 'core_message, Serialize_core_message },\n'
    textSerializeSource = '\
namespace {\n\
\n\
using Types = QVector<' + typeIdType + '>;\n\
using StagesFlags = QVector<int32>;\n\
\n\
' + textSerializeMethods + '\n\
\n\
using TextSerializer = bool (*)(DumpToTextBuffer &to, int32 stage, int32 lev, Types &types, Types &vtypes, StagesFlags &stages, StagesFlags &flags, const ' + primeType + ' *start, const ' + primeType + ' *end, uint32 iflag);\n\
\n\
base::flat_map<' + typeIdType + ', TextSerializer> CreateTextSerializers() {\n\
	return {\n\
' + textSerializeInit + '\n\
	};\n\
}\n\
\n\
} // namespace\n\
\n\
bool DumpToTextType(DumpToTextBuffer &to, const ' + primeType + ' *&from, const ' + primeType + ' *end, ' + primeType + ' cons, uint32 level, ' + primeType + ' vcons) {\n\
	static auto kSerializers = CreateTextSerializers();\n\
\n\
	QVector<' + typeIdType + '> types, vtypes;\n\
	QVector<int32> stages, flags;\n\
	types.reserve(20); vtypes.reserve(20); stages.reserve(20); flags.reserve(20);\n\
	types.push_back(' + typeIdType + '(cons)); vtypes.push_back(' + typeIdType + '(vcons)); stages.push_back(0); flags.push_back(0);\n\
\n\
	' + typeIdType + ' type = cons, vtype = vcons;\n\
	int32 stage = 0, flag = 0;\n\
\n\
	while (!types.isEmpty()) {\n\
		type = types.back();\n\
		vtype = vtypes.back();\n\
		stage = stages.back();\n\
		flag = flags.back();\n\
		if (!type) {\n\
			if (from >= end) {\n\
				to.error("insufficient data");\n\
				return false;\n\
			} else if (stage) {\n\
				to.error("unknown type on stage > 0");\n\
				return false;\n\
			}\n\
			types.back() = type = *from;\n\
			++from;\n\
		}\n\
\n\
		int32 lev = level + types.size() - 1;\n\
		auto it = kSerializers.find(type);\n\
		if (it != kSerializers.end()) {\n\
			if (!(*it->second)(to, stage, lev, types, vtypes, stages, flags, from, end, flag)) {\n\
				to.error();\n\
				return false;\n\
			}\n\
		} else if (DumpToTextCore(to, from, end, type, lev, vtype)) {\n\
			types.pop_back(); vtypes.pop_back(); stages.pop_back(); flags.pop_back();\n\
		} else {\n\
			to.error();\n\
			return false;\n\
		}\n\
	}\n\
	return true;\n\
}\n'

  # module itself
  header = '\
// WARNING! All changes made in this file will be lost!\n\
// Created from ' + inputNames + ' by \'generate.py\'\n\
//\n\
#pragma once\n\
\n\
' + ('#include "' + builtinInclude + '"\n' if builtinInclude != '' else '') + '\
#include "base/assertion.h"\n\
#include "base/flags.h"\n\
#include "tl/tl_boxed.h"\n\
#include "tl/tl_type_owner.h"\n\
\n\
' + ('namespace ' + globalNamespace + ' {\n' if globalNamespace != '' else '') + '\
' + ('namespace ' + creatorNamespace + ' {\n' if creatorNamespace != '' else '') + '\
\n\
' + ('inline constexpr auto kCurrentLayer = ' + primeType + '(' + str(layer) + ');\n\n' if layer != 0 else '') +'\
class TypeCreator;\n\
\n\
' + ('} // namespace ' + creatorNamespace + '\n\n' if creatorNamespace != '' else '') + '\
// Type id constants\n\
enum {\n\
' + ',\n'.join(enums) + '\n\
};\n\
\n\
// Type forward declarations\n\
' + forwards + '\n\
// Boxed types definitions\n\
' + forwTypedefs + '\n\
// Type classes definitions\n\
' + typesText + '\n\
// Type constructors with data\n\
' + dataTexts + '\n\
// RPC methods\n\
' + funcsText + '\n\
// Template methods definition\n\
' + inlineMethods + '\n\
// Visitor definition\n\
' + visitorMethods + '\n\
// Flag operators definition\n\
' + flagOperators + '\n\
// Factory methods declaration\n\
' + factories + '\n\
' + ('} // namespace ' + globalNamespace + '\n' if globalNamespace != '' else '')

  source = '\
// WARNING! All changes made in this file will be lost!\n\
// Created from ' + inputNames + ' by \'generate.py\'\n\
//\n\
#include "' + outputHeaderBasename + '"\n\
\n\
// Creator proxy class definition\n\
' + ('namespace ' + globalNamespace + ' {\n' if globalNamespace != '' else '') + '\
' + ('namespace ' + creatorNamespace + ' {\n' if creatorNamespace != '' else '') + '\
\n\
class TypeCreator final {\n\
public:\n\
' + creatorProxyText + '\n\
};\n\
\n\
' + ('} // namespace ' + creatorNamespace + '\n\n' if creatorNamespace != '' else '') + '\
// Methods definition\n\
' + methods + '\n\
' + ('} // namespace ' + globalNamespace + '\n' if globalNamespace != '' else '')

  conversionHeader = '\
// WARNING! All changes made in this file will be lost!\n\
// Created from ' + inputNames + ' by \'generate.py\'\n\
//\n\
#pragma once\n\
\n\
#include "' + conversionInclude + '"\n\
#include "' + outputHeaderBasename + '"\n\
\n\
' + ('namespace ' + globalNamespace + ' {\n\n' if globalNamespace != '' else '') + '\
' + conversionHeader + '\n\
' + ('} // namespace ' + globalNamespace + '\n' if globalNamespace != '' else '') +'\
' + ('\n#include "' + conversionBuiltinInclude + '"\n' if conversionBuiltinInclude != '' else '')

  conversionSource = '\
// WARNING! All changes made in this file will be lost!\n\
// Created from ' + inputNames + ' by \'generate.py\'\n\
//\n\
#include "' + outputConversionHeaderBasename + '"\n\
\n\
' + ('namespace ' + globalNamespace + ' {\n' if globalNamespace != '' else '') + '\
' + conversionSource + '\n\
' + ('} // namespace ' + globalNamespace + '\n' if globalNamespace != '' else '')

  serializationHeader = '\
// WARNING! All changes made in this file will be lost!\n\
// Created from ' + inputNames + ' by \'generate.py\'\n\
//\n\
#pragma once\n\
\n\
' + ('#include "' + builtinInclude + '"\n\n' if builtinInclude != '' else '') + '\
namespace MTP::details {\n\
\n\
struct DumpToTextBuffer;\n\
\n\
[[nodiscard]] bool DumpToTextType(DumpToTextBuffer &to, const ' + primeType + ' *&from, const ' + primeType + ' *end, ' + primeType + ' cons = 0, uint32 level = 0, ' + primeType + ' vcons = 0);\n\
\n\
} // namespace MTP::details\n'

  serializationSource = '\
// WARNING! All changes made in this file will be lost!\n\
// Created from ' + inputNames + ' by \'generate.py\'\n\
//\n\
#include "' + outputSerializationHeaderBasename + '"\n\
#include "' + outputHeaderBasename + '"\n\
#include "' + serializationInclude + '"\n\
#include "base/flat_map.h"\n\
\n\
namespace MTP::details {\n\
' + textSerializeSource + '\n\
} // namespace MTP::details\n'

  alreadyHeader = ''
  if os.path.isfile(outputHeader):
    with open(outputHeader, 'r') as already:
      alreadyHeader = already.read()
  if alreadyHeader != header:
    with open(outputHeader, 'w') as out:
      out.write(header)

  alreadySource = ''
  if os.path.isfile(outputSource):
    with open(outputSource, 'r') as already:
      alreadySource = already.read()
  if alreadySource != source:
    with open(outputSource, 'w') as out:
      out.write(source)

  if writeConversion:
    alreadyHeader = ''
    if os.path.isfile(outputConversionHeader):
      with open(outputConversionHeader, 'r') as already:
        alreadyHeader = already.read()
    if alreadyHeader != conversionHeader:
      with open(outputConversionHeader, 'w') as out:
        out.write(conversionHeader)

    alreadySource = ''
    if os.path.isfile(outputConversionSource):
      with open(outputConversionSource, 'r') as already:
        alreadySource = already.read()
    if alreadySource != conversionSource:
      with open(outputConversionSource, 'w') as out:
        out.write(conversionSource)

  if writeSerialization:
    alreadyHeader = ''
    if os.path.isfile(outputSerializationHeader):
      with open(outputSerializationHeader, 'r') as already:
        alreadyHeader = already.read()
    if alreadyHeader != serializationHeader:
      with open(outputSerializationHeader, 'w') as out:
        out.write(serializationHeader)

    alreadySource = ''
    if os.path.isfile(outputSerializationSource):
      with open(outputSerializationSource, 'r') as already:
        alreadySource = already.read()
    if alreadySource != serializationSource:
      with open(outputSerializationSource, 'w') as out:
        out.write(serializationSource)

  with open(outputPath + '.timestamp', 'w') as out:
    out.write('1')
