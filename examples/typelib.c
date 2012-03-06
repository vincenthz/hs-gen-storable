enum {
  BLOB_TYPE_INVALID,
  BLOB_TYPE_FUNCTION,
  BLOB_TYPE_CALLBACK,
  BLOB_TYPE_STRUCT,
  BLOB_TYPE_BOXED,
  BLOB_TYPE_ENUM,
  BLOB_TYPE_FLAGS,
  BLOB_TYPE_OBJECT,
  BLOB_TYPE_INTERFACE,
  BLOB_TYPE_CONSTANT,
  BLOB_TYPE_INVALID_0,
  BLOB_TYPE_UNION
} GTypelibBlobType;

struct {
  uint8_t  major_version;
  uint8_t  minor_version;
  uint16_t reserved;
  uint16_t n_entries;
  uint16_t n_local_entries;
  uint32_t directory;
  uint32_t n_attributes;
  uint32_t attributes;

  uint32_t dependencies;

  uint32_t size;
  uint32_t namespace;
  uint32_t nsversion;
  uint32_t shared_library;
  uint32_t c_prefix;

  uint16_t entry_blob_size;
  uint16_t function_blob_size;
  uint16_t callback_blob_size;
  uint16_t signal_blob_size;
  uint16_t vfunc_blob_size;
  uint16_t arg_blob_size;
  uint16_t property_blob_size;
  uint16_t field_blob_size;
  uint16_t value_blob_size;
  uint16_t attribute_blob_size;
  uint16_t constant_blob_size;
  uint16_t error_domain_blob_size;

  uint16_t signature_blob_size;
  uint16_t enum_blob_size;
  uint16_t struct_blob_size;
  uint16_t object_blob_size;
  uint16_t interface_blob_size;
  uint16_t union_blob_size;

  uint32_t sections;
  uint8_t padding[6];
} Header;

enum {
  GI_SECTION_END,
  GI_SECTION_DIRECTORY_INDEX
} SectionType;

struct {
  uint32_t id;
  uint32_t offset;
} Section;

struct {
  uint16_t blob_type;

  uint16_t local    : 1; /* & 1 */
  uint16_t reserved :15 #hidden; /* >> 1  & 2^15-1*/
  uint32_t name;
  uint32_t offset;
} DirEntry;

/*
union
{
  struct
  {
    uint32_t reserved   : 8;
    uint32_t reserved2  :16;
    uint32_t pointer    : 1;
    uint32_t reserved3  : 2;
    uint32_t tag        : 5;
  } flags;
  uint32_t    offset;
} SimpleTypeBlob;
*/

struct { uint32_t offset; } SimpleTypeBlob;


struct {
  uint32_t        name;

  uint32_t          in                           : 1;
  uint32_t          out                          : 1;
  uint32_t          caller_allocates             : 1;
  uint32_t          allow_none                   : 1;
  uint32_t          optional                     : 1;
  uint32_t          transfer_ownership           : 1;
  uint32_t          transfer_container_ownership : 1;
  uint32_t          return_value                 : 1;
  uint32_t          scope                        : 3;
  uint32_t          skip                         : 1;
  uint32_t          reserved                     :20 #hidden;
  int8_t        closure;
  int8_t        destroy;
  uint16_t      padding;

  SimpleTypeBlob arg_type;
} ArgBlob;

struct {
  SimpleTypeBlob return_type;

  uint16_t        may_return_null              : 1;
  uint16_t        caller_owns_return_value     : 1;
  uint16_t        caller_owns_return_container : 1;
  uint16_t        skip_return                  : 1;
  uint16_t        reserved                     :12 #hidden;

  uint16_t        n_arguments #hidden;

  ArgBlob        arguments[@n_arguments];
} SignatureBlob;

struct {
  uint16_t blob_type;

  uint16_t deprecated : 1;
  uint16_t reserved   :15 #hidden;
  uint32_t name;
} CommonBlob;

struct {
  uint16_t blob_type;

  uint16_t deprecated  : 1;
  uint16_t setter      : 1;
  uint16_t getter      : 1;
  uint16_t constructor : 1;
  uint16_t wraps_vfunc : 1;
  uint16_t throws      : 1;
  uint16_t index       :10;

  uint32_t name;
  uint32_t symbol;
  uint32_t signature;

  uint16_t is_static   : 1;
  uint16_t reserved    : 15 #hidden;
  uint16_t reserved2 #hidden;
} FunctionBlob;

struct {
  uint16_t blob_type;

  uint16_t deprecated : 1;
  uint16_t reserved   :15 #hidden;
  uint32_t name;
  uint32_t signature;
} CallbackBlob;

struct {
  uint8_t  pointer  :1;
  uint8_t  reserved :2 #hidden;
  uint8_t  tag      :5;
  uint8_t  reserved2 #hidden;
  uint16_t interface;
} InterfaceTypeBlob;

struct {
  uint16_t pointer         :1;
  uint16_t reserved        :2 #hidden;
  uint16_t tag             :5;

  uint16_t zero_terminated :1;
  uint16_t has_length      :1;
  uint16_t has_size        :1;
  uint16_t array_type      :2;
  uint16_t reserved2       :3 #hidden;

  uint16_t dimensions; /* union { uint16_t length; uint16_t size } */

  SimpleTypeBlob type;
} ArrayTypeBlob;

struct {
  uint8_t	 pointer  :1;
  uint8_t	 reserved :2 #hidden;
  uint8_t	 tag      :5;

  uint8_t	 reserved2 #hidden;
  uint16_t	 n_types #hidden;

  SimpleTypeBlob type[@n_types];
} ParamTypeBlob;

struct {
  uint8_t  pointer  :1;
  uint8_t  reserved :2 #hidden;
  uint8_t  tag      :5;

  uint8_t  reserved2 #hidden;

  uint16_t n_domains #hidden;
  uint16_t domains[@n_domains];
}  ErrorTypeBlob;

struct {
  uint32_t deprecated : 1;
  uint32_t unsigned_value : 1;
  uint32_t reserved   :30 #hidden;
  uint32_t name;
  int32_t value;
} ValueBlob;

struct {
  uint16_t blob_type;
  uint16_t deprecated   : 1;
  uint16_t unregistered : 1;
  uint16_t reserved :14 #hidden;
  uint32_t name;

  uint32_t gtype_name;
  uint32_t gtype_init;
} RegisteredTypeBlob;

struct {
  uint32_t        name;

  uint8_t         readable :1;
  uint8_t         writable :1;
  uint8_t         has_embedded_type :1;
  uint8_t         reserved :5 #hidden;
  uint8_t         bits;

  uint16_t        struct_offset;

  uint32_t        reserved2 #hidden;

  SimpleTypeBlob type;
} FieldBlob;

struct {
  uint16_t   blob_type;

  uint16_t   deprecated   : 1;
  uint16_t   unregistered : 1;
  uint16_t   is_gtype_struct : 1;
  uint16_t   alignment    : 6;
  uint16_t   foreign      : 1;
  uint16_t   reserved     : 6 #hidden;

  uint32_t   name;

  uint32_t   gtype_name;
  uint32_t   gtype_init;

  uint32_t   size;

  uint16_t   n_fields #hidden;
  uint16_t   n_methods #hidden;

  uint32_t   reserved2 #hidden;
  uint32_t   reserved3 #hidden;

  FieldBlob    fields[@n_fields];
  FunctionBlob methods[@n_methods];
} StructBlob;

struct {
  uint16_t        blob_type;
  uint16_t        deprecated   : 1;
  uint16_t        reserved     :15 #hidden;
  uint32_t        name;

  SimpleTypeBlob type;

  uint32_t        size;
  uint32_t        offset;

  uint32_t        reserved2 #hidden;
} ConstantBlob;

struct {
  uint16_t      blob_type;
  uint16_t      deprecated    : 1;
  uint16_t      unregistered  : 1;
  uint16_t      discriminated : 1;
  uint16_t      alignment     : 6;
  uint16_t      reserved      : 7 #hidden;
  uint32_t      name;

  uint32_t      gtype_name;
  uint32_t      gtype_init;

  uint32_t      size;

  uint16_t      n_fields #hidden;
  uint16_t      n_functions #hidden;

  uint32_t      reserved2 #hidden;
  uint32_t      reserved3 #hidden;

  int32_t       discriminator_offset;
  SimpleTypeBlob discriminator_type;

  FieldBlob    fields[@n_fields];
  FunctionBlob functions[@n_functions];
  //ConstantBlob discriminator_values[0];
} UnionBlob;

struct {
  uint16_t   blob_type;

  uint16_t   deprecated   : 1;
  uint16_t   unregistered : 1;
  uint16_t   storage_type : 5;
  uint16_t   reserved     : 9;

  uint32_t   name;

  uint32_t   gtype_name;
  uint32_t   gtype_init;

  uint16_t   n_values #hidden;
  uint16_t   n_methods #hidden;

  uint32_t   error_domain;

  ValueBlob values[@n_values];
  FunctionBlob methods[@n_methods];
} EnumBlob;

struct {
  uint32_t        name;

  uint32_t        deprecated                   : 1;
  uint32_t        readable                     : 1;
  uint32_t        writable                     : 1;
  uint32_t        construct                    : 1;
  uint32_t        construct_only               : 1;
  uint32_t        transfer_ownership           : 1;
  uint32_t        transfer_container_ownership : 1;
  uint32_t        reserved                     :25 #hidden;

  uint32_t        reserved2 #hidden;

  SimpleTypeBlob type;
} PropertyBlob;

struct {
  uint16_t deprecated        : 1;
  uint16_t run_first         : 1;
  uint16_t run_last          : 1;
  uint16_t run_cleanup       : 1;
  uint16_t no_recurse        : 1;
  uint16_t detailed          : 1;
  uint16_t action            : 1;
  uint16_t no_hooks          : 1;
  uint16_t has_class_closure : 1;
  uint16_t true_stops_emit   : 1;
  uint16_t reserved          : 6 #hidden;

  uint16_t class_closure;

  uint32_t name;

  uint32_t reserved2 #hidden;

  uint32_t signature;
} SignalBlob;

struct {
  uint32_t name;

  uint16_t must_chain_up           : 1;
  uint16_t must_be_implemented     : 1;
  uint16_t must_not_be_implemented : 1;
  uint16_t class_closure           : 1;
  uint16_t reserved                :12 #hidden;
  uint16_t signal;

  uint16_t struct_offset;
  uint16_t invoker : 10;
  uint16_t reserved2 : 6 #hidden;

  uint32_t reserved3 #hidden;
  uint32_t signature;
} VFuncBlob;

struct {
  uint16_t   blob_type;
  uint16_t   deprecated   : 1;
  uint16_t   abstract     : 1;
  uint16_t   fundamental  : 1;
  uint16_t   reserved     :13 #hidden;
  uint32_t   name;

  uint32_t   gtype_name;
  uint32_t   gtype_init;

  uint16_t   parent;
  uint16_t   gtype_struct;

  uint16_t   n_interfaces #hidden;
  uint16_t   n_fields #hidden;
  uint16_t   n_properties #hidden;
  uint16_t   n_methods #hidden;
  uint16_t   n_signals #hidden;
  uint16_t   n_vfuncs #hidden;
  uint16_t   n_constants #hidden;
  uint16_t   reserved2 #hidden;

  uint32_t   ref_func;
  uint32_t   unref_func;
  uint32_t   set_value_func;
  uint32_t   get_value_func;

  uint32_t   reserved3 #hidden;
  uint32_t   reserved4 #hidden;

  uint16_t   interfaces[@n_interfaces];

  FieldBlob           fields[@n_fields];
  PropertyBlob        properties[@n_properties];
  FunctionBlob        methods[@n_methods];
  SignalBlob          signals[@n_signals];
  VFuncBlob           vfuncs[@n_vfuncs];
  ConstantBlob        constants[@n_constants];
} ObjectBlob;

struct {
  uint16_t blob_type;
  uint16_t deprecated   : 1;
  uint16_t reserved     :15 #hidden;
  uint32_t name;

  uint32_t gtype_name;
  uint32_t gtype_init;
  uint16_t gtype_struct;

  uint16_t n_prerequisites #hidden;
  uint16_t n_properties #hidden;
  uint16_t n_methods #hidden;
  uint16_t n_signals #hidden;
  uint16_t n_vfuncs #hidden;
  uint16_t n_constants #hidden;

  uint16_t padding #hidden;

  uint32_t reserved2 #hidden;
  uint32_t reserved3 #hidden;

  uint16_t prerequisites[@n_prerequisites];

  PropertyBlob        properties[@n_properties];
  FunctionBlob        methods[@n_methods];
  SignalBlob          signals[@n_signals];
  VFuncBlob           vfuncs[@n_vfuncs];
  ConstantBlob        constants[@n_constants];
} InterfaceBlob;

struct {
  uint32_t offset;
  uint32_t name;
  uint32_t value;
} AttributeBlob;
