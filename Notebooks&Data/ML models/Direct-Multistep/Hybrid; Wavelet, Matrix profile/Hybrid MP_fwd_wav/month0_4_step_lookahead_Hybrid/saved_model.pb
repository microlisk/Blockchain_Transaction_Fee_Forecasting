зќ.
 л
D
AddV2
x"T
y"T
z"T"
Ttype:
2	ђљ
^
AssignVariableOp
resource
value"dtype"
dtypetype"
validate_shapebool( ѕ
~
BiasAdd

value"T	
bias"T
output"T" 
Ttype:
2	"-
data_formatstringNHWC:
NHWCNCHW
8
Const
output"dtype"
valuetensor"
dtypetype
^
Fill
dims"
index_type

value"T
output"T"	
Ttype"

index_typetype0:
2	
.
Identity

input"T
output"T"	
Ttype
q
MatMul
a"T
b"T
product"T"
transpose_abool( "
transpose_bbool( "
Ttype:

2	
e
MergeV2Checkpoints
checkpoint_prefixes
destination_prefix"
delete_old_dirsbool(ѕ
?
Mul
x"T
y"T
z"T"
Ttype:
2	љ

NoOp
M
Pack
values"T*N
output"T"
Nint(0"	
Ttype"
axisint 
│
PartitionedCall
args2Tin
output2Tout"
Tin
list(type)("
Tout
list(type)("	
ffunc"
configstring "
config_protostring "
executor_typestring 
C
Placeholder
output"dtype"
dtypetype"
shapeshape:
@
ReadVariableOp
resource
value"dtype"
dtypetypeѕ
o
	RestoreV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0ѕ
l
SaveV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0ѕ
?
Select
	condition

t"T
e"T
output"T"	
Ttype
P
Shape

input"T
output"out_type"	
Ttype"
out_typetype0:
2	
H
ShardedFilename
basename	
shard

num_shards
filename
0
Sigmoid
x"T
y"T"
Ttype:

2
[
Split
	split_dim

value"T
output"T*	num_split"
	num_splitint(0"	
Ttype
┴
StatefulPartitionedCall
args2Tin
output2Tout"
Tin
list(type)("
Tout
list(type)("	
ffunc"
configstring "
config_protostring "
executor_typestring ѕе
џ
StatelessWhile

input2T
output2T"
T
list(type)("
condfunc"
bodyfunc" 
output_shapeslist(shape)
 "
parallel_iterationsint

@
StaticRegexFullMatch	
input

output
"
patternstring
Ш
StridedSlice

input"T
begin"Index
end"Index
strides"Index
output"T"	
Ttype"
Indextype:
2	"

begin_maskint "
end_maskint "
ellipsis_maskint "
new_axis_maskint "
shrink_axis_maskint 
N

StringJoin
inputs*N

output"
Nint(0"
	separatorstring 
-
Tanh
x"T
y"T"
Ttype:

2
░
TensorListFromTensor
tensor"element_dtype
element_shape"
shape_type/
output_handleіжУelement_dtype"
element_dtypetype"

shape_typetype:
2	
Ъ
TensorListReserve
element_shape"
shape_type
num_elements(
handleіжУelement_dtype"
element_dtypetype"

shape_typetype:
2	
ѕ
TensorListStack
input_handle
element_shape
tensor"element_dtype"
element_dtypetype" 
num_elementsint         
P
	Transpose
x"T
perm"Tperm
y"T"	
Ttype"
Tpermtype0:
2	
ќ
VarHandleOp
resource"
	containerstring "
shared_namestring "
dtypetype"
shapeshape"#
allowed_deviceslist(string)
 ѕ"serve*2.8.02v2.8.0-rc1-32-g3f878cff5b68Іб-
x
dense_3/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*
shared_namedense_3/kernel
q
"dense_3/kernel/Read/ReadVariableOpReadVariableOpdense_3/kernel*
_output_shapes

:*
dtype0
p
dense_3/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*
shared_namedense_3/bias
i
 dense_3/bias/Read/ReadVariableOpReadVariableOpdense_3/bias*
_output_shapes
:*
dtype0
f
	Adam/iterVarHandleOp*
_output_shapes
: *
dtype0	*
shape: *
shared_name	Adam/iter
_
Adam/iter/Read/ReadVariableOpReadVariableOp	Adam/iter*
_output_shapes
: *
dtype0	
j
Adam/beta_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nameAdam/beta_1
c
Adam/beta_1/Read/ReadVariableOpReadVariableOpAdam/beta_1*
_output_shapes
: *
dtype0
j
Adam/beta_2VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nameAdam/beta_2
c
Adam/beta_2/Read/ReadVariableOpReadVariableOpAdam/beta_2*
_output_shapes
: *
dtype0
h

Adam/decayVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name
Adam/decay
a
Adam/decay/Read/ReadVariableOpReadVariableOp
Adam/decay*
_output_shapes
: *
dtype0
x
Adam/learning_rateVarHandleOp*
_output_shapes
: *
dtype0*
shape: *#
shared_nameAdam/learning_rate
q
&Adam/learning_rate/Read/ReadVariableOpReadVariableOpAdam/learning_rate*
_output_shapes
: *
dtype0
ј
lstm_3/lstm_cell_3/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:<**
shared_namelstm_3/lstm_cell_3/kernel
Є
-lstm_3/lstm_cell_3/kernel/Read/ReadVariableOpReadVariableOplstm_3/lstm_cell_3/kernel*
_output_shapes

:<*
dtype0
б
#lstm_3/lstm_cell_3/recurrent_kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:<*4
shared_name%#lstm_3/lstm_cell_3/recurrent_kernel
Џ
7lstm_3/lstm_cell_3/recurrent_kernel/Read/ReadVariableOpReadVariableOp#lstm_3/lstm_cell_3/recurrent_kernel*
_output_shapes

:<*
dtype0
є
lstm_3/lstm_cell_3/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:<*(
shared_namelstm_3/lstm_cell_3/bias

+lstm_3/lstm_cell_3/bias/Read/ReadVariableOpReadVariableOplstm_3/lstm_cell_3/bias*
_output_shapes
:<*
dtype0
^
totalVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nametotal
W
total/Read/ReadVariableOpReadVariableOptotal*
_output_shapes
: *
dtype0
^
countVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_namecount
W
count/Read/ReadVariableOpReadVariableOpcount*
_output_shapes
: *
dtype0
є
Adam/dense_3/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*&
shared_nameAdam/dense_3/kernel/m

)Adam/dense_3/kernel/m/Read/ReadVariableOpReadVariableOpAdam/dense_3/kernel/m*
_output_shapes

:*
dtype0
~
Adam/dense_3/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:*$
shared_nameAdam/dense_3/bias/m
w
'Adam/dense_3/bias/m/Read/ReadVariableOpReadVariableOpAdam/dense_3/bias/m*
_output_shapes
:*
dtype0
ю
 Adam/lstm_3/lstm_cell_3/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
:<*1
shared_name" Adam/lstm_3/lstm_cell_3/kernel/m
Ћ
4Adam/lstm_3/lstm_cell_3/kernel/m/Read/ReadVariableOpReadVariableOp Adam/lstm_3/lstm_cell_3/kernel/m*
_output_shapes

:<*
dtype0
░
*Adam/lstm_3/lstm_cell_3/recurrent_kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
:<*;
shared_name,*Adam/lstm_3/lstm_cell_3/recurrent_kernel/m
Е
>Adam/lstm_3/lstm_cell_3/recurrent_kernel/m/Read/ReadVariableOpReadVariableOp*Adam/lstm_3/lstm_cell_3/recurrent_kernel/m*
_output_shapes

:<*
dtype0
ћ
Adam/lstm_3/lstm_cell_3/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:<*/
shared_name Adam/lstm_3/lstm_cell_3/bias/m
Ї
2Adam/lstm_3/lstm_cell_3/bias/m/Read/ReadVariableOpReadVariableOpAdam/lstm_3/lstm_cell_3/bias/m*
_output_shapes
:<*
dtype0
є
Adam/dense_3/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*&
shared_nameAdam/dense_3/kernel/v

)Adam/dense_3/kernel/v/Read/ReadVariableOpReadVariableOpAdam/dense_3/kernel/v*
_output_shapes

:*
dtype0
~
Adam/dense_3/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:*$
shared_nameAdam/dense_3/bias/v
w
'Adam/dense_3/bias/v/Read/ReadVariableOpReadVariableOpAdam/dense_3/bias/v*
_output_shapes
:*
dtype0
ю
 Adam/lstm_3/lstm_cell_3/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
:<*1
shared_name" Adam/lstm_3/lstm_cell_3/kernel/v
Ћ
4Adam/lstm_3/lstm_cell_3/kernel/v/Read/ReadVariableOpReadVariableOp Adam/lstm_3/lstm_cell_3/kernel/v*
_output_shapes

:<*
dtype0
░
*Adam/lstm_3/lstm_cell_3/recurrent_kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
:<*;
shared_name,*Adam/lstm_3/lstm_cell_3/recurrent_kernel/v
Е
>Adam/lstm_3/lstm_cell_3/recurrent_kernel/v/Read/ReadVariableOpReadVariableOp*Adam/lstm_3/lstm_cell_3/recurrent_kernel/v*
_output_shapes

:<*
dtype0
ћ
Adam/lstm_3/lstm_cell_3/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:<*/
shared_name Adam/lstm_3/lstm_cell_3/bias/v
Ї
2Adam/lstm_3/lstm_cell_3/bias/v/Read/ReadVariableOpReadVariableOpAdam/lstm_3/lstm_cell_3/bias/v*
_output_shapes
:<*
dtype0

NoOpNoOp
║(
ConstConst"/device:CPU:0*
_output_shapes
: *
dtype0*ш'
valueв'BУ' Bр'
џ
layer_with_weights-0
layer-0
layer_with_weights-1
layer-1
	optimizer
	variables
trainable_variables
regularization_losses
	keras_api
__call__
*	&call_and_return_all_conditional_losses

_default_save_signature

signatures*
┴
cell

state_spec
	variables
trainable_variables
regularization_losses
	keras_api
_random_generator
__call__
*&call_and_return_all_conditional_losses*
д

kernel
bias
	variables
trainable_variables
regularization_losses
	keras_api
__call__
*&call_and_return_all_conditional_losses*
ъ
iter

beta_1

beta_2
	 decay
!learning_ratemHmI"mJ#mK$mLvMvN"vO#vP$vQ*
'
"0
#1
$2
3
4*
'
"0
#1
$2
3
4*
* 
░
%non_trainable_variables

&layers
'metrics
(layer_regularization_losses
)layer_metrics
	variables
trainable_variables
regularization_losses
__call__

_default_save_signature
*	&call_and_return_all_conditional_losses
&	"call_and_return_conditional_losses*
* 
* 
* 

*serving_default* 
с
+
state_size

"kernel
#recurrent_kernel
$bias
,	variables
-trainable_variables
.regularization_losses
/	keras_api
0_random_generator
1__call__
*2&call_and_return_all_conditional_losses*
* 

"0
#1
$2*

"0
#1
$2*
* 
Ъ

3states
4non_trainable_variables

5layers
6metrics
7layer_regularization_losses
8layer_metrics
	variables
trainable_variables
regularization_losses
__call__
*&call_and_return_all_conditional_losses
&"call_and_return_conditional_losses*
* 
* 
* 
^X
VARIABLE_VALUEdense_3/kernel6layer_with_weights-1/kernel/.ATTRIBUTES/VARIABLE_VALUE*
ZT
VARIABLE_VALUEdense_3/bias4layer_with_weights-1/bias/.ATTRIBUTES/VARIABLE_VALUE*

0
1*

0
1*
* 
Њ
9non_trainable_variables

:layers
;metrics
<layer_regularization_losses
=layer_metrics
	variables
trainable_variables
regularization_losses
__call__
*&call_and_return_all_conditional_losses
&"call_and_return_conditional_losses*
* 
* 
LF
VARIABLE_VALUE	Adam/iter)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUE*
PJ
VARIABLE_VALUEAdam/beta_1+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUE*
PJ
VARIABLE_VALUEAdam/beta_2+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUE*
NH
VARIABLE_VALUE
Adam/decay*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUE*
^X
VARIABLE_VALUEAdam/learning_rate2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUE*
YS
VARIABLE_VALUElstm_3/lstm_cell_3/kernel&variables/0/.ATTRIBUTES/VARIABLE_VALUE*
c]
VARIABLE_VALUE#lstm_3/lstm_cell_3/recurrent_kernel&variables/1/.ATTRIBUTES/VARIABLE_VALUE*
WQ
VARIABLE_VALUElstm_3/lstm_cell_3/bias&variables/2/.ATTRIBUTES/VARIABLE_VALUE*
* 

0
1*

>0*
* 
* 
* 
* 

"0
#1
$2*

"0
#1
$2*
* 
Њ
?non_trainable_variables

@layers
Ametrics
Blayer_regularization_losses
Clayer_metrics
,	variables
-trainable_variables
.regularization_losses
1__call__
*2&call_and_return_all_conditional_losses
&2"call_and_return_conditional_losses*
* 
* 
* 
* 
* 

0*
* 
* 
* 
* 
* 
* 
* 
* 
8
	Dtotal
	Ecount
F	variables
G	keras_api*
* 
* 
* 
* 
* 
SM
VARIABLE_VALUEtotal4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUE*
SM
VARIABLE_VALUEcount4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUE*

D0
E1*

F	variables*
Ђ{
VARIABLE_VALUEAdam/dense_3/kernel/mRlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
}w
VARIABLE_VALUEAdam/dense_3/bias/mPlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
|v
VARIABLE_VALUE Adam/lstm_3/lstm_cell_3/kernel/mBvariables/0/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
Єђ
VARIABLE_VALUE*Adam/lstm_3/lstm_cell_3/recurrent_kernel/mBvariables/1/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
zt
VARIABLE_VALUEAdam/lstm_3/lstm_cell_3/bias/mBvariables/2/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
Ђ{
VARIABLE_VALUEAdam/dense_3/kernel/vRlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
}w
VARIABLE_VALUEAdam/dense_3/bias/vPlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
|v
VARIABLE_VALUE Adam/lstm_3/lstm_cell_3/kernel/vBvariables/0/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
Єђ
VARIABLE_VALUE*Adam/lstm_3/lstm_cell_3/recurrent_kernel/vBvariables/1/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
zt
VARIABLE_VALUEAdam/lstm_3/lstm_cell_3/bias/vBvariables/2/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
Ѕ
serving_default_lstm_3_inputPlaceholder*,
_output_shapes
:         Я*
dtype0*!
shape:         Я
║
StatefulPartitionedCallStatefulPartitionedCallserving_default_lstm_3_inputlstm_3/lstm_cell_3/kernel#lstm_3/lstm_cell_3/recurrent_kernellstm_3/lstm_cell_3/biasdense_3/kerneldense_3/bias*
Tin

2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:         *'
_read_only_resource_inputs	
*0
config_proto 

CPU

GPU2*0J 8ѓ *,
f'R%
#__inference_signature_wrapper_77344
O
saver_filenamePlaceholder*
_output_shapes
: *
dtype0*
shape: 
ж	
StatefulPartitionedCall_1StatefulPartitionedCallsaver_filename"dense_3/kernel/Read/ReadVariableOp dense_3/bias/Read/ReadVariableOpAdam/iter/Read/ReadVariableOpAdam/beta_1/Read/ReadVariableOpAdam/beta_2/Read/ReadVariableOpAdam/decay/Read/ReadVariableOp&Adam/learning_rate/Read/ReadVariableOp-lstm_3/lstm_cell_3/kernel/Read/ReadVariableOp7lstm_3/lstm_cell_3/recurrent_kernel/Read/ReadVariableOp+lstm_3/lstm_cell_3/bias/Read/ReadVariableOptotal/Read/ReadVariableOpcount/Read/ReadVariableOp)Adam/dense_3/kernel/m/Read/ReadVariableOp'Adam/dense_3/bias/m/Read/ReadVariableOp4Adam/lstm_3/lstm_cell_3/kernel/m/Read/ReadVariableOp>Adam/lstm_3/lstm_cell_3/recurrent_kernel/m/Read/ReadVariableOp2Adam/lstm_3/lstm_cell_3/bias/m/Read/ReadVariableOp)Adam/dense_3/kernel/v/Read/ReadVariableOp'Adam/dense_3/bias/v/Read/ReadVariableOp4Adam/lstm_3/lstm_cell_3/kernel/v/Read/ReadVariableOp>Adam/lstm_3/lstm_cell_3/recurrent_kernel/v/Read/ReadVariableOp2Adam/lstm_3/lstm_cell_3/bias/v/Read/ReadVariableOpConst*#
Tin
2	*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8ѓ *'
f"R 
__inference__traced_save_79284
г
StatefulPartitionedCall_2StatefulPartitionedCallsaver_filenamedense_3/kerneldense_3/bias	Adam/iterAdam/beta_1Adam/beta_2
Adam/decayAdam/learning_ratelstm_3/lstm_cell_3/kernel#lstm_3/lstm_cell_3/recurrent_kernellstm_3/lstm_cell_3/biastotalcountAdam/dense_3/kernel/mAdam/dense_3/bias/m Adam/lstm_3/lstm_cell_3/kernel/m*Adam/lstm_3/lstm_cell_3/recurrent_kernel/mAdam/lstm_3/lstm_cell_3/bias/mAdam/dense_3/kernel/vAdam/dense_3/bias/v Adam/lstm_3/lstm_cell_3/kernel/v*Adam/lstm_3/lstm_cell_3/recurrent_kernel/vAdam/lstm_3/lstm_cell_3/bias/v*"
Tin
2*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8ѓ **
f%R#
!__inference__traced_restore_79360ии,
│
И
A__inference_lstm_3_layer_call_and_return_conditional_losses_74839

inputs.
read_readvariableop_resource:<0
read_1_readvariableop_resource:<,
read_2_readvariableop_resource:<

identity_3ѕбRead/ReadVariableOpбRead_1/ReadVariableOpбRead_2/ReadVariableOp;
ShapeShapeinputs*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Л
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskP
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :s
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:P
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    l
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:         R
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :w
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:R
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    r
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:         E
ones_like/ShapeShapeinputs*
T0*
_output_shapes
:T
ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ђ?ё
	ones_likeFillones_like/Shape:output:0ones_like/Const:output:0*
T0*4
_output_shapes"
 :                  e
mulMulinputsones_like:output:0*
T0*4
_output_shapes"
 :                  p
Read/ReadVariableOpReadVariableOpread_readvariableop_resource*
_output_shapes

:<*
dtype0Z
IdentityIdentityRead/ReadVariableOp:value:0*
T0*
_output_shapes

:<t
Read_1/ReadVariableOpReadVariableOpread_1_readvariableop_resource*
_output_shapes

:<*
dtype0^

Identity_1IdentityRead_1/ReadVariableOp:value:0*
T0*
_output_shapes

:<p
Read_2/ReadVariableOpReadVariableOpread_2_readvariableop_resource*
_output_shapes
:<*
dtype0Z

Identity_2IdentityRead_2/ReadVariableOp:value:0*
T0*
_output_shapes
:<Й
PartitionedCallPartitionedCallmul:z:0zeros:output:0zeros_1:output:0Identity:output:0Identity_1:output:0Identity_2:output:0*
Tin

2*
Tout	
2*
_collective_manager_ids
 *o
_output_shapes]
[:         :                  :         :         : * 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8ѓ *(
f#R!
__inference_standard_lstm_74566i

Identity_3IdentityPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         ї
NoOpNoOp^Read/ReadVariableOp^Read_1/ReadVariableOp^Read_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:                  : : : 2*
Read/ReadVariableOpRead/ReadVariableOp2.
Read_1/ReadVariableOpRead_1/ReadVariableOp2.
Read_2/ReadVariableOpRead_2/ReadVariableOp:\ X
4
_output_shapes"
 :                  
 
_user_specified_nameinputs
ї	
╝
while_cond_74954
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_74954___redundant_placeholder03
/while_while_cond_74954___redundant_placeholder13
/while_while_cond_74954___redundant_placeholder23
/while_while_cond_74954___redundant_placeholder3
while_identity
`

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: K
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: ")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*S
_input_shapesB
@: : : : :         :         : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
::

_output_shapes
::	

_output_shapes
::


_output_shapes
:
Ж┴
ы
9__inference___backward_gpu_lstm_with_fallback_75581_75757
placeholder
placeholder_1
placeholder_2
placeholder_3
placeholder_4/
+gradients_strided_slice_grad_shape_cudnnrnnA
=gradients_transpose_9_grad_invertpermutation_transpose_9_perm)
%gradients_squeeze_grad_shape_cudnnrnn+
'gradients_squeeze_1_grad_shape_cudnnrnn!
gradients_zeros_like_cudnnrnn6
2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose7
3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims9
5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_15
1gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1=
9gradients_transpose_grad_invertpermutation_transpose_perm*
&gradients_expanddims_grad_shape_init_h,
(gradients_expanddims_1_grad_shape_init_c-
)gradients_concat_1_grad_mod_concat_1_axisA
=gradients_transpose_1_grad_invertpermutation_transpose_1_permA
=gradients_transpose_2_grad_invertpermutation_transpose_2_permA
=gradients_transpose_3_grad_invertpermutation_transpose_3_permA
=gradients_transpose_4_grad_invertpermutation_transpose_4_permA
=gradients_transpose_5_grad_invertpermutation_transpose_5_permA
=gradients_transpose_6_grad_invertpermutation_transpose_6_permA
=gradients_transpose_7_grad_invertpermutation_transpose_7_permA
=gradients_transpose_8_grad_invertpermutation_transpose_8_perm3
/gradients_split_2_grad_concat_split_2_split_dim/
+gradients_split_grad_concat_split_split_dim3
/gradients_split_1_grad_concat_split_1_split_dim)
%gradients_concat_grad_mod_concat_axis
identity

identity_1

identity_2

identity_3

identity_4

identity_5ѕ^
gradients/grad_ys_0Identityplaceholder*
T0*'
_output_shapes
:         e
gradients/grad_ys_1Identityplaceholder_1*
T0*,
_output_shapes
:         Я`
gradients/grad_ys_2Identityplaceholder_2*
T0*'
_output_shapes
:         `
gradients/grad_ys_3Identityplaceholder_3*
T0*'
_output_shapes
:         O
gradients/grad_ys_4Identityplaceholder_4*
T0*
_output_shapes
: }
"gradients/strided_slice_grad/ShapeShape+gradients_strided_slice_grad_shape_cudnnrnn*
T0*
_output_shapes
:є
3gradients/strided_slice_grad/StridedSliceGrad/beginConst*
_output_shapes
:*
dtype0*
valueB:
         {
1gradients/strided_slice_grad/StridedSliceGrad/endConst*
_output_shapes
:*
dtype0*
valueB: 
5gradients/strided_slice_grad/StridedSliceGrad/stridesConst*
_output_shapes
:*
dtype0*
valueB:б
-gradients/strided_slice_grad/StridedSliceGradStridedSliceGrad+gradients/strided_slice_grad/Shape:output:0<gradients/strided_slice_grad/StridedSliceGrad/begin:output:0:gradients/strided_slice_grad/StridedSliceGrad/end:output:0>gradients/strided_slice_grad/StridedSliceGrad/strides:output:0gradients/grad_ys_0:output:0*
Index0*
T0*,
_output_shapes
:Я         *
shrink_axis_maskю
,gradients/transpose_9_grad/InvertPermutationInvertPermutation=gradients_transpose_9_grad_invertpermutation_transpose_9_perm*
_output_shapes
:И
$gradients/transpose_9_grad/transpose	Transposegradients/grad_ys_1:output:00gradients/transpose_9_grad/InvertPermutation:y:0*
T0*,
_output_shapes
:Я         q
gradients/Squeeze_grad/ShapeShape%gradients_squeeze_grad_shape_cudnnrnn*
T0*
_output_shapes
:ц
gradients/Squeeze_grad/ReshapeReshapegradients/grad_ys_2:output:0%gradients/Squeeze_grad/Shape:output:0*
T0*+
_output_shapes
:         u
gradients/Squeeze_1_grad/ShapeShape'gradients_squeeze_1_grad_shape_cudnnrnn*
T0*
_output_shapes
:е
 gradients/Squeeze_1_grad/ReshapeReshapegradients/grad_ys_3:output:0'gradients/Squeeze_1_grad/Shape:output:0*
T0*+
_output_shapes
:         Щ
gradients/AddNAddN6gradients/strided_slice_grad/StridedSliceGrad:output:0(gradients/transpose_9_grad/transpose:y:0*
N*
T0*@
_class6
42loc:@gradients/strided_slice_grad/StridedSliceGrad*,
_output_shapes
:Я         c
gradients/zeros_like	ZerosLikegradients_zeros_like_cudnnrnn*
T0*
_output_shapes
:Ђ
(gradients/CudnnRNN_grad/CudnnRNNBackpropCudnnRNNBackprop2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_11gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1+gradients_strided_slice_grad_shape_cudnnrnn%gradients_squeeze_grad_shape_cudnnrnn'gradients_squeeze_1_grad_shape_cudnnrnngradients/AddN:sum:0'gradients/Squeeze_grad/Reshape:output:0)gradients/Squeeze_1_grad/Reshape:output:0gradients_zeros_like_cudnnrnn*
T0*a
_output_shapesO
M:Я         :         :         :Зќ
*gradients/transpose_grad/InvertPermutationInvertPermutation9gradients_transpose_grad_invertpermutation_transpose_perm*
_output_shapes
:Л
"gradients/transpose_grad/transpose	Transpose9gradients/CudnnRNN_grad/CudnnRNNBackprop:input_backprop:0.gradients/transpose_grad/InvertPermutation:y:0*
T0*,
_output_shapes
:         Яu
gradients/ExpandDims_grad/ShapeShape&gradients_expanddims_grad_shape_init_h*
T0*
_output_shapes
:┼
!gradients/ExpandDims_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_h_backprop:0(gradients/ExpandDims_grad/Shape:output:0*
T0*'
_output_shapes
:         y
!gradients/ExpandDims_1_grad/ShapeShape(gradients_expanddims_1_grad_shape_init_c*
T0*
_output_shapes
:╔
#gradients/ExpandDims_1_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_c_backprop:0*gradients/ExpandDims_1_grad/Shape:output:0*
T0*'
_output_shapes
:         ^
gradients/concat_1_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :џ
gradients/concat_1_grad/modFloorMod)gradients_concat_1_grad_mod_concat_1_axis%gradients/concat_1_grad/Rank:output:0*
T0*
_output_shapes
: g
gradients/concat_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_2Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_3Const*
_output_shapes
:*
dtype0*
valueB:j
gradients/concat_1_grad/Shape_4Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_5Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_6Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_7Const*
_output_shapes
:*
dtype0*
valueB:рi
gradients/concat_1_grad/Shape_8Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_9Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_10Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_11Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_12Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_13Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_14Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_15Const*
_output_shapes
:*
dtype0*
valueB:Э
$gradients/concat_1_grad/ConcatOffsetConcatOffsetgradients/concat_1_grad/mod:z:0&gradients/concat_1_grad/Shape:output:0(gradients/concat_1_grad/Shape_1:output:0(gradients/concat_1_grad/Shape_2:output:0(gradients/concat_1_grad/Shape_3:output:0(gradients/concat_1_grad/Shape_4:output:0(gradients/concat_1_grad/Shape_5:output:0(gradients/concat_1_grad/Shape_6:output:0(gradients/concat_1_grad/Shape_7:output:0(gradients/concat_1_grad/Shape_8:output:0(gradients/concat_1_grad/Shape_9:output:0)gradients/concat_1_grad/Shape_10:output:0)gradients/concat_1_grad/Shape_11:output:0)gradients/concat_1_grad/Shape_12:output:0)gradients/concat_1_grad/Shape_13:output:0)gradients/concat_1_grad/Shape_14:output:0)gradients/concat_1_grad/Shape_15:output:0*
N*t
_output_shapesb
`::::::::::::::::в
gradients/concat_1_grad/SliceSlice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:0&gradients/concat_1_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_1Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:1(gradients/concat_1_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_2Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:2(gradients/concat_1_grad/Shape_2:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_3Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:3(gradients/concat_1_grad/Shape_3:output:0*
Index0*
T0*
_output_shapes
:­
gradients/concat_1_grad/Slice_4Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:4(gradients/concat_1_grad/Shape_4:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_5Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:5(gradients/concat_1_grad/Shape_5:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_6Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:6(gradients/concat_1_grad/Shape_6:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_7Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:7(gradients/concat_1_grad/Shape_7:output:0*
Index0*
T0*
_output_shapes	
:р№
gradients/concat_1_grad/Slice_8Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:8(gradients/concat_1_grad/Shape_8:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_9Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:9(gradients/concat_1_grad/Shape_9:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_10Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:10)gradients/concat_1_grad/Shape_10:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_11Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:11)gradients/concat_1_grad/Shape_11:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_12Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:12)gradients/concat_1_grad/Shape_12:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_13Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:13)gradients/concat_1_grad/Shape_13:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_14Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:14)gradients/concat_1_grad/Shape_14:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_15Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:15)gradients/concat_1_grad/Shape_15:output:0*
Index0*
T0*
_output_shapes
:m
gradients/Reshape_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      А
gradients/Reshape_grad/ReshapeReshape&gradients/concat_1_grad/Slice:output:0%gradients/Reshape_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_1_grad/ReshapeReshape(gradients/concat_1_grad/Slice_1:output:0'gradients/Reshape_1_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_2_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_2_grad/ReshapeReshape(gradients/concat_1_grad/Slice_2:output:0'gradients/Reshape_2_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_3_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_3_grad/ReshapeReshape(gradients/concat_1_grad/Slice_3:output:0'gradients/Reshape_3_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_4_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_4_grad/ReshapeReshape(gradients/concat_1_grad/Slice_4:output:0'gradients/Reshape_4_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_5_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_5_grad/ReshapeReshape(gradients/concat_1_grad/Slice_5:output:0'gradients/Reshape_5_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_6_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_6_grad/ReshapeReshape(gradients/concat_1_grad/Slice_6:output:0'gradients/Reshape_6_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_7_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_7_grad/ReshapeReshape(gradients/concat_1_grad/Slice_7:output:0'gradients/Reshape_7_grad/Shape:output:0*
T0*
_output_shapes

:h
gradients/Reshape_8_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_8_grad/ReshapeReshape(gradients/concat_1_grad/Slice_8:output:0'gradients/Reshape_8_grad/Shape:output:0*
T0*
_output_shapes
:h
gradients/Reshape_9_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_9_grad/ReshapeReshape(gradients/concat_1_grad/Slice_9:output:0'gradients/Reshape_9_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_10_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_10_grad/ReshapeReshape)gradients/concat_1_grad/Slice_10:output:0(gradients/Reshape_10_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_11_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_11_grad/ReshapeReshape)gradients/concat_1_grad/Slice_11:output:0(gradients/Reshape_11_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_12_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_12_grad/ReshapeReshape)gradients/concat_1_grad/Slice_12:output:0(gradients/Reshape_12_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_13_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_13_grad/ReshapeReshape)gradients/concat_1_grad/Slice_13:output:0(gradients/Reshape_13_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_14_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_14_grad/ReshapeReshape)gradients/concat_1_grad/Slice_14:output:0(gradients/Reshape_14_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_15_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_15_grad/ReshapeReshape)gradients/concat_1_grad/Slice_15:output:0(gradients/Reshape_15_grad/Shape:output:0*
T0*
_output_shapes
:ю
,gradients/transpose_1_grad/InvertPermutationInvertPermutation=gradients_transpose_1_grad_invertpermutation_transpose_1_perm*
_output_shapes
:х
$gradients/transpose_1_grad/transpose	Transpose'gradients/Reshape_grad/Reshape:output:00gradients/transpose_1_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_2_grad/InvertPermutationInvertPermutation=gradients_transpose_2_grad_invertpermutation_transpose_2_perm*
_output_shapes
:и
$gradients/transpose_2_grad/transpose	Transpose)gradients/Reshape_1_grad/Reshape:output:00gradients/transpose_2_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_3_grad/InvertPermutationInvertPermutation=gradients_transpose_3_grad_invertpermutation_transpose_3_perm*
_output_shapes
:и
$gradients/transpose_3_grad/transpose	Transpose)gradients/Reshape_2_grad/Reshape:output:00gradients/transpose_3_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_4_grad/InvertPermutationInvertPermutation=gradients_transpose_4_grad_invertpermutation_transpose_4_perm*
_output_shapes
:и
$gradients/transpose_4_grad/transpose	Transpose)gradients/Reshape_3_grad/Reshape:output:00gradients/transpose_4_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_5_grad/InvertPermutationInvertPermutation=gradients_transpose_5_grad_invertpermutation_transpose_5_perm*
_output_shapes
:и
$gradients/transpose_5_grad/transpose	Transpose)gradients/Reshape_4_grad/Reshape:output:00gradients/transpose_5_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_6_grad/InvertPermutationInvertPermutation=gradients_transpose_6_grad_invertpermutation_transpose_6_perm*
_output_shapes
:и
$gradients/transpose_6_grad/transpose	Transpose)gradients/Reshape_5_grad/Reshape:output:00gradients/transpose_6_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_7_grad/InvertPermutationInvertPermutation=gradients_transpose_7_grad_invertpermutation_transpose_7_perm*
_output_shapes
:и
$gradients/transpose_7_grad/transpose	Transpose)gradients/Reshape_6_grad/Reshape:output:00gradients/transpose_7_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_8_grad/InvertPermutationInvertPermutation=gradients_transpose_8_grad_invertpermutation_transpose_8_perm*
_output_shapes
:и
$gradients/transpose_8_grad/transpose	Transpose)gradients/Reshape_7_grad/Reshape:output:00gradients/transpose_8_grad/InvertPermutation:y:0*
T0*
_output_shapes

:Т
gradients/split_2_grad/concatConcatV2)gradients/Reshape_8_grad/Reshape:output:0)gradients/Reshape_9_grad/Reshape:output:0*gradients/Reshape_10_grad/Reshape:output:0*gradients/Reshape_11_grad/Reshape:output:0*gradients/Reshape_12_grad/Reshape:output:0*gradients/Reshape_13_grad/Reshape:output:0*gradients/Reshape_14_grad/Reshape:output:0*gradients/Reshape_15_grad/Reshape:output:0/gradients_split_2_grad_concat_split_2_split_dim*
N*
T0*
_output_shapes
:x«
gradients/split_grad/concatConcatV2(gradients/transpose_1_grad/transpose:y:0(gradients/transpose_2_grad/transpose:y:0(gradients/transpose_3_grad/transpose:y:0(gradients/transpose_4_grad/transpose:y:0+gradients_split_grad_concat_split_split_dim*
N*
T0*
_output_shapes

:<┤
gradients/split_1_grad/concatConcatV2(gradients/transpose_5_grad/transpose:y:0(gradients/transpose_6_grad/transpose:y:0(gradients/transpose_7_grad/transpose:y:0(gradients/transpose_8_grad/transpose:y:0/gradients_split_1_grad_concat_split_1_split_dim*
N*
T0*
_output_shapes

:<\
gradients/concat_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :њ
gradients/concat_grad/modFloorMod%gradients_concat_grad_mod_concat_axis#gradients/concat_grad/Rank:output:0*
T0*
_output_shapes
: e
gradients/concat_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:<g
gradients/concat_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:<╩
"gradients/concat_grad/ConcatOffsetConcatOffsetgradients/concat_grad/mod:z:0$gradients/concat_grad/Shape:output:0&gradients/concat_grad/Shape_1:output:0*
N* 
_output_shapes
::Л
gradients/concat_grad/SliceSlice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:0$gradients/concat_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:<Н
gradients/concat_grad/Slice_1Slice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:1&gradients/concat_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:<s
IdentityIdentity&gradients/transpose_grad/transpose:y:0*
T0*,
_output_shapes
:         Яt

Identity_1Identity*gradients/ExpandDims_grad/Reshape:output:0*
T0*'
_output_shapes
:         v

Identity_2Identity,gradients/ExpandDims_1_grad/Reshape:output:0*
T0*'
_output_shapes
:         e

Identity_3Identity$gradients/split_grad/concat:output:0*
T0*
_output_shapes

:<g

Identity_4Identity&gradients/split_1_grad/concat:output:0*
T0*
_output_shapes

:<c

Identity_5Identity&gradients/concat_grad/Slice_1:output:0*
T0*
_output_shapes
:<"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"!

identity_5Identity_5:output:0*(
_construction_contextkEagerRuntime*в
_input_shapes┘
о:         :         Я:         :         : :Я         ::         :         ::Я         :         :         :З::         :         : ::::::::: : : : *=
api_implements+)lstm_d78c62b6-1045-479a-bb0d-b6d9b57a4016*
api_preferred_deviceGPU*A
forward_function_name(&__forward_gpu_lstm_with_fallback_75756*
go_backwards( *

time_major( :- )
'
_output_shapes
:         :2.
,
_output_shapes
:         Я:-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :2.
,
_output_shapes
:Я         : 

_output_shapes
::1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :	

_output_shapes
::2
.
,
_output_shapes
:Я         :1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :!

_output_shapes	
:З: 

_output_shapes
::-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: : 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
::

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
Њ:
┐
__inference_standard_lstm_76010

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4c
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          n
	transpose	Transposeinputstranspose/perm:output:0*
T0*,
_output_shapes
:Я         B
ShapeShapetranspose:y:0*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Л
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskf
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
         ▓
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмє
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Я
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУм_
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:ж
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maskd
MatMulMatMulstrided_slice_1:output:0kernel*
T0*'
_output_shapes
:         <^
MatMul_1MatMulinit_hrecurrent_kernel*
T0*'
_output_shapes
:         <d
addAddV2MatMul:product:0MatMul_1:product:0*
T0*'
_output_shapes
:         <S
BiasAddBiasAddadd:z:0bias*
T0*'
_output_shapes
:         <Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :Х
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_splitT
SigmoidSigmoidsplit:output:0*
T0*'
_output_shapes
:         V
	Sigmoid_1Sigmoidsplit:output:1*
T0*'
_output_shapes
:         S
mulMulSigmoid_1:y:0init_c*
T0*'
_output_shapes
:         N
TanhTanhsplit:output:2*
T0*'
_output_shapes
:         U
mul_1MulSigmoid:y:0Tanh:y:0*
T0*'
_output_shapes
:         T
add_1AddV2mul:z:0	mul_1:z:0*
T0*'
_output_shapes
:         V
	Sigmoid_2Sigmoidsplit:output:3*
T0*'
_output_shapes
:         K
Tanh_1Tanh	add_1:z:0*
T0*'
_output_shapes
:         Y
mul_2MulSigmoid_2:y:0
Tanh_1:y:0*
T0*'
_output_shapes
:         n
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Х
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмF
timeConst*
_output_shapes
: *
dtype0*
value	B : c
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
         T
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : «
whileStatelessWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0init_hinit_cstrided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0kernelrecurrent_kernelbias*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*`
_output_shapesN
L: : : : :         :         : : :<:<:<* 
_read_only_resource_inputs
 *
_stateful_parallelism( *
bodyR
while_body_75925*
condR
while_cond_75924*_
output_shapesN
L: : : : :         :         : : :<:<:<*
parallel_iterations Ђ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       ├
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*,
_output_shapes
:Я         *
element_dtype0h
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
         a
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Є
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          Ќ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*,
_output_shapes
:         Я[
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *  ђ?`
IdentityIdentitystrided_slice_2:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_1:y:0*
T0*,
_output_shapes
:         ЯX

Identity_2Identitywhile:output:4*
T0*'
_output_shapes
:         X

Identity_3Identitywhile:output:5*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_017ba245-194f-42f6-9988-067e14309e69*
api_preferred_deviceCPU*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
а@
╦
(__inference_gpu_lstm_with_fallback_78534

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4ѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          n
	transpose	Transposeinputstranspose/perm:output:0*
T0*,
_output_shapes
:Я         P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : o

ExpandDims
ExpandDimsinit_hExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : s
ExpandDims_1
ExpandDimsinit_cExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : Ъ
concat_1ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0*
_output_shapes	
:З╦
CudnnRNNCudnnRNNtranspose:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1:output:0*
T0*^
_output_shapesL
J:Я         :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          }
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*,
_output_shapes
:         Яp
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_9:y:0*
T0*,
_output_shapes
:         ЯZ

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_c1189ae0-4eec-42a2-962a-5abd558aa8a4*
api_preferred_deviceGPU*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
а@
╦
(__inference_gpu_lstm_with_fallback_76673

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4ѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          n
	transpose	Transposeinputstranspose/perm:output:0*
T0*,
_output_shapes
:Я         P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : o

ExpandDims
ExpandDimsinit_hExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : s
ExpandDims_1
ExpandDimsinit_cExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : Ъ
concat_1ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0*
_output_shapes	
:З╦
CudnnRNNCudnnRNNtranspose:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1:output:0*
T0*^
_output_shapesL
J:Я         :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          }
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*,
_output_shapes
:         Яp
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_9:y:0*
T0*,
_output_shapes
:         ЯZ

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_a92946bc-3bab-4632-a518-695c88a7828f*
api_preferred_deviceGPU*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
Ж┴
ы
9__inference___backward_gpu_lstm_with_fallback_78535_78711
placeholder
placeholder_1
placeholder_2
placeholder_3
placeholder_4/
+gradients_strided_slice_grad_shape_cudnnrnnA
=gradients_transpose_9_grad_invertpermutation_transpose_9_perm)
%gradients_squeeze_grad_shape_cudnnrnn+
'gradients_squeeze_1_grad_shape_cudnnrnn!
gradients_zeros_like_cudnnrnn6
2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose7
3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims9
5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_15
1gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1=
9gradients_transpose_grad_invertpermutation_transpose_perm*
&gradients_expanddims_grad_shape_init_h,
(gradients_expanddims_1_grad_shape_init_c-
)gradients_concat_1_grad_mod_concat_1_axisA
=gradients_transpose_1_grad_invertpermutation_transpose_1_permA
=gradients_transpose_2_grad_invertpermutation_transpose_2_permA
=gradients_transpose_3_grad_invertpermutation_transpose_3_permA
=gradients_transpose_4_grad_invertpermutation_transpose_4_permA
=gradients_transpose_5_grad_invertpermutation_transpose_5_permA
=gradients_transpose_6_grad_invertpermutation_transpose_6_permA
=gradients_transpose_7_grad_invertpermutation_transpose_7_permA
=gradients_transpose_8_grad_invertpermutation_transpose_8_perm3
/gradients_split_2_grad_concat_split_2_split_dim/
+gradients_split_grad_concat_split_split_dim3
/gradients_split_1_grad_concat_split_1_split_dim)
%gradients_concat_grad_mod_concat_axis
identity

identity_1

identity_2

identity_3

identity_4

identity_5ѕ^
gradients/grad_ys_0Identityplaceholder*
T0*'
_output_shapes
:         e
gradients/grad_ys_1Identityplaceholder_1*
T0*,
_output_shapes
:         Я`
gradients/grad_ys_2Identityplaceholder_2*
T0*'
_output_shapes
:         `
gradients/grad_ys_3Identityplaceholder_3*
T0*'
_output_shapes
:         O
gradients/grad_ys_4Identityplaceholder_4*
T0*
_output_shapes
: }
"gradients/strided_slice_grad/ShapeShape+gradients_strided_slice_grad_shape_cudnnrnn*
T0*
_output_shapes
:є
3gradients/strided_slice_grad/StridedSliceGrad/beginConst*
_output_shapes
:*
dtype0*
valueB:
         {
1gradients/strided_slice_grad/StridedSliceGrad/endConst*
_output_shapes
:*
dtype0*
valueB: 
5gradients/strided_slice_grad/StridedSliceGrad/stridesConst*
_output_shapes
:*
dtype0*
valueB:б
-gradients/strided_slice_grad/StridedSliceGradStridedSliceGrad+gradients/strided_slice_grad/Shape:output:0<gradients/strided_slice_grad/StridedSliceGrad/begin:output:0:gradients/strided_slice_grad/StridedSliceGrad/end:output:0>gradients/strided_slice_grad/StridedSliceGrad/strides:output:0gradients/grad_ys_0:output:0*
Index0*
T0*,
_output_shapes
:Я         *
shrink_axis_maskю
,gradients/transpose_9_grad/InvertPermutationInvertPermutation=gradients_transpose_9_grad_invertpermutation_transpose_9_perm*
_output_shapes
:И
$gradients/transpose_9_grad/transpose	Transposegradients/grad_ys_1:output:00gradients/transpose_9_grad/InvertPermutation:y:0*
T0*,
_output_shapes
:Я         q
gradients/Squeeze_grad/ShapeShape%gradients_squeeze_grad_shape_cudnnrnn*
T0*
_output_shapes
:ц
gradients/Squeeze_grad/ReshapeReshapegradients/grad_ys_2:output:0%gradients/Squeeze_grad/Shape:output:0*
T0*+
_output_shapes
:         u
gradients/Squeeze_1_grad/ShapeShape'gradients_squeeze_1_grad_shape_cudnnrnn*
T0*
_output_shapes
:е
 gradients/Squeeze_1_grad/ReshapeReshapegradients/grad_ys_3:output:0'gradients/Squeeze_1_grad/Shape:output:0*
T0*+
_output_shapes
:         Щ
gradients/AddNAddN6gradients/strided_slice_grad/StridedSliceGrad:output:0(gradients/transpose_9_grad/transpose:y:0*
N*
T0*@
_class6
42loc:@gradients/strided_slice_grad/StridedSliceGrad*,
_output_shapes
:Я         c
gradients/zeros_like	ZerosLikegradients_zeros_like_cudnnrnn*
T0*
_output_shapes
:Ђ
(gradients/CudnnRNN_grad/CudnnRNNBackpropCudnnRNNBackprop2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_11gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1+gradients_strided_slice_grad_shape_cudnnrnn%gradients_squeeze_grad_shape_cudnnrnn'gradients_squeeze_1_grad_shape_cudnnrnngradients/AddN:sum:0'gradients/Squeeze_grad/Reshape:output:0)gradients/Squeeze_1_grad/Reshape:output:0gradients_zeros_like_cudnnrnn*
T0*a
_output_shapesO
M:Я         :         :         :Зќ
*gradients/transpose_grad/InvertPermutationInvertPermutation9gradients_transpose_grad_invertpermutation_transpose_perm*
_output_shapes
:Л
"gradients/transpose_grad/transpose	Transpose9gradients/CudnnRNN_grad/CudnnRNNBackprop:input_backprop:0.gradients/transpose_grad/InvertPermutation:y:0*
T0*,
_output_shapes
:         Яu
gradients/ExpandDims_grad/ShapeShape&gradients_expanddims_grad_shape_init_h*
T0*
_output_shapes
:┼
!gradients/ExpandDims_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_h_backprop:0(gradients/ExpandDims_grad/Shape:output:0*
T0*'
_output_shapes
:         y
!gradients/ExpandDims_1_grad/ShapeShape(gradients_expanddims_1_grad_shape_init_c*
T0*
_output_shapes
:╔
#gradients/ExpandDims_1_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_c_backprop:0*gradients/ExpandDims_1_grad/Shape:output:0*
T0*'
_output_shapes
:         ^
gradients/concat_1_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :џ
gradients/concat_1_grad/modFloorMod)gradients_concat_1_grad_mod_concat_1_axis%gradients/concat_1_grad/Rank:output:0*
T0*
_output_shapes
: g
gradients/concat_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_2Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_3Const*
_output_shapes
:*
dtype0*
valueB:j
gradients/concat_1_grad/Shape_4Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_5Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_6Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_7Const*
_output_shapes
:*
dtype0*
valueB:рi
gradients/concat_1_grad/Shape_8Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_9Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_10Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_11Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_12Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_13Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_14Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_15Const*
_output_shapes
:*
dtype0*
valueB:Э
$gradients/concat_1_grad/ConcatOffsetConcatOffsetgradients/concat_1_grad/mod:z:0&gradients/concat_1_grad/Shape:output:0(gradients/concat_1_grad/Shape_1:output:0(gradients/concat_1_grad/Shape_2:output:0(gradients/concat_1_grad/Shape_3:output:0(gradients/concat_1_grad/Shape_4:output:0(gradients/concat_1_grad/Shape_5:output:0(gradients/concat_1_grad/Shape_6:output:0(gradients/concat_1_grad/Shape_7:output:0(gradients/concat_1_grad/Shape_8:output:0(gradients/concat_1_grad/Shape_9:output:0)gradients/concat_1_grad/Shape_10:output:0)gradients/concat_1_grad/Shape_11:output:0)gradients/concat_1_grad/Shape_12:output:0)gradients/concat_1_grad/Shape_13:output:0)gradients/concat_1_grad/Shape_14:output:0)gradients/concat_1_grad/Shape_15:output:0*
N*t
_output_shapesb
`::::::::::::::::в
gradients/concat_1_grad/SliceSlice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:0&gradients/concat_1_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_1Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:1(gradients/concat_1_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_2Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:2(gradients/concat_1_grad/Shape_2:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_3Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:3(gradients/concat_1_grad/Shape_3:output:0*
Index0*
T0*
_output_shapes
:­
gradients/concat_1_grad/Slice_4Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:4(gradients/concat_1_grad/Shape_4:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_5Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:5(gradients/concat_1_grad/Shape_5:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_6Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:6(gradients/concat_1_grad/Shape_6:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_7Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:7(gradients/concat_1_grad/Shape_7:output:0*
Index0*
T0*
_output_shapes	
:р№
gradients/concat_1_grad/Slice_8Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:8(gradients/concat_1_grad/Shape_8:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_9Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:9(gradients/concat_1_grad/Shape_9:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_10Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:10)gradients/concat_1_grad/Shape_10:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_11Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:11)gradients/concat_1_grad/Shape_11:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_12Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:12)gradients/concat_1_grad/Shape_12:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_13Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:13)gradients/concat_1_grad/Shape_13:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_14Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:14)gradients/concat_1_grad/Shape_14:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_15Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:15)gradients/concat_1_grad/Shape_15:output:0*
Index0*
T0*
_output_shapes
:m
gradients/Reshape_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      А
gradients/Reshape_grad/ReshapeReshape&gradients/concat_1_grad/Slice:output:0%gradients/Reshape_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_1_grad/ReshapeReshape(gradients/concat_1_grad/Slice_1:output:0'gradients/Reshape_1_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_2_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_2_grad/ReshapeReshape(gradients/concat_1_grad/Slice_2:output:0'gradients/Reshape_2_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_3_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_3_grad/ReshapeReshape(gradients/concat_1_grad/Slice_3:output:0'gradients/Reshape_3_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_4_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_4_grad/ReshapeReshape(gradients/concat_1_grad/Slice_4:output:0'gradients/Reshape_4_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_5_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_5_grad/ReshapeReshape(gradients/concat_1_grad/Slice_5:output:0'gradients/Reshape_5_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_6_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_6_grad/ReshapeReshape(gradients/concat_1_grad/Slice_6:output:0'gradients/Reshape_6_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_7_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_7_grad/ReshapeReshape(gradients/concat_1_grad/Slice_7:output:0'gradients/Reshape_7_grad/Shape:output:0*
T0*
_output_shapes

:h
gradients/Reshape_8_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_8_grad/ReshapeReshape(gradients/concat_1_grad/Slice_8:output:0'gradients/Reshape_8_grad/Shape:output:0*
T0*
_output_shapes
:h
gradients/Reshape_9_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_9_grad/ReshapeReshape(gradients/concat_1_grad/Slice_9:output:0'gradients/Reshape_9_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_10_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_10_grad/ReshapeReshape)gradients/concat_1_grad/Slice_10:output:0(gradients/Reshape_10_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_11_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_11_grad/ReshapeReshape)gradients/concat_1_grad/Slice_11:output:0(gradients/Reshape_11_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_12_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_12_grad/ReshapeReshape)gradients/concat_1_grad/Slice_12:output:0(gradients/Reshape_12_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_13_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_13_grad/ReshapeReshape)gradients/concat_1_grad/Slice_13:output:0(gradients/Reshape_13_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_14_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_14_grad/ReshapeReshape)gradients/concat_1_grad/Slice_14:output:0(gradients/Reshape_14_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_15_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_15_grad/ReshapeReshape)gradients/concat_1_grad/Slice_15:output:0(gradients/Reshape_15_grad/Shape:output:0*
T0*
_output_shapes
:ю
,gradients/transpose_1_grad/InvertPermutationInvertPermutation=gradients_transpose_1_grad_invertpermutation_transpose_1_perm*
_output_shapes
:х
$gradients/transpose_1_grad/transpose	Transpose'gradients/Reshape_grad/Reshape:output:00gradients/transpose_1_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_2_grad/InvertPermutationInvertPermutation=gradients_transpose_2_grad_invertpermutation_transpose_2_perm*
_output_shapes
:и
$gradients/transpose_2_grad/transpose	Transpose)gradients/Reshape_1_grad/Reshape:output:00gradients/transpose_2_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_3_grad/InvertPermutationInvertPermutation=gradients_transpose_3_grad_invertpermutation_transpose_3_perm*
_output_shapes
:и
$gradients/transpose_3_grad/transpose	Transpose)gradients/Reshape_2_grad/Reshape:output:00gradients/transpose_3_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_4_grad/InvertPermutationInvertPermutation=gradients_transpose_4_grad_invertpermutation_transpose_4_perm*
_output_shapes
:и
$gradients/transpose_4_grad/transpose	Transpose)gradients/Reshape_3_grad/Reshape:output:00gradients/transpose_4_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_5_grad/InvertPermutationInvertPermutation=gradients_transpose_5_grad_invertpermutation_transpose_5_perm*
_output_shapes
:и
$gradients/transpose_5_grad/transpose	Transpose)gradients/Reshape_4_grad/Reshape:output:00gradients/transpose_5_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_6_grad/InvertPermutationInvertPermutation=gradients_transpose_6_grad_invertpermutation_transpose_6_perm*
_output_shapes
:и
$gradients/transpose_6_grad/transpose	Transpose)gradients/Reshape_5_grad/Reshape:output:00gradients/transpose_6_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_7_grad/InvertPermutationInvertPermutation=gradients_transpose_7_grad_invertpermutation_transpose_7_perm*
_output_shapes
:и
$gradients/transpose_7_grad/transpose	Transpose)gradients/Reshape_6_grad/Reshape:output:00gradients/transpose_7_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_8_grad/InvertPermutationInvertPermutation=gradients_transpose_8_grad_invertpermutation_transpose_8_perm*
_output_shapes
:и
$gradients/transpose_8_grad/transpose	Transpose)gradients/Reshape_7_grad/Reshape:output:00gradients/transpose_8_grad/InvertPermutation:y:0*
T0*
_output_shapes

:Т
gradients/split_2_grad/concatConcatV2)gradients/Reshape_8_grad/Reshape:output:0)gradients/Reshape_9_grad/Reshape:output:0*gradients/Reshape_10_grad/Reshape:output:0*gradients/Reshape_11_grad/Reshape:output:0*gradients/Reshape_12_grad/Reshape:output:0*gradients/Reshape_13_grad/Reshape:output:0*gradients/Reshape_14_grad/Reshape:output:0*gradients/Reshape_15_grad/Reshape:output:0/gradients_split_2_grad_concat_split_2_split_dim*
N*
T0*
_output_shapes
:x«
gradients/split_grad/concatConcatV2(gradients/transpose_1_grad/transpose:y:0(gradients/transpose_2_grad/transpose:y:0(gradients/transpose_3_grad/transpose:y:0(gradients/transpose_4_grad/transpose:y:0+gradients_split_grad_concat_split_split_dim*
N*
T0*
_output_shapes

:<┤
gradients/split_1_grad/concatConcatV2(gradients/transpose_5_grad/transpose:y:0(gradients/transpose_6_grad/transpose:y:0(gradients/transpose_7_grad/transpose:y:0(gradients/transpose_8_grad/transpose:y:0/gradients_split_1_grad_concat_split_1_split_dim*
N*
T0*
_output_shapes

:<\
gradients/concat_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :њ
gradients/concat_grad/modFloorMod%gradients_concat_grad_mod_concat_axis#gradients/concat_grad/Rank:output:0*
T0*
_output_shapes
: e
gradients/concat_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:<g
gradients/concat_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:<╩
"gradients/concat_grad/ConcatOffsetConcatOffsetgradients/concat_grad/mod:z:0$gradients/concat_grad/Shape:output:0&gradients/concat_grad/Shape_1:output:0*
N* 
_output_shapes
::Л
gradients/concat_grad/SliceSlice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:0$gradients/concat_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:<Н
gradients/concat_grad/Slice_1Slice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:1&gradients/concat_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:<s
IdentityIdentity&gradients/transpose_grad/transpose:y:0*
T0*,
_output_shapes
:         Яt

Identity_1Identity*gradients/ExpandDims_grad/Reshape:output:0*
T0*'
_output_shapes
:         v

Identity_2Identity,gradients/ExpandDims_1_grad/Reshape:output:0*
T0*'
_output_shapes
:         e

Identity_3Identity$gradients/split_grad/concat:output:0*
T0*
_output_shapes

:<g

Identity_4Identity&gradients/split_1_grad/concat:output:0*
T0*
_output_shapes

:<c

Identity_5Identity&gradients/concat_grad/Slice_1:output:0*
T0*
_output_shapes
:<"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"!

identity_5Identity_5:output:0*(
_construction_contextkEagerRuntime*в
_input_shapes┘
о:         :         Я:         :         : :Я         ::         :         ::Я         :         :         :З::         :         : ::::::::: : : : *=
api_implements+)lstm_c1189ae0-4eec-42a2-962a-5abd558aa8a4*
api_preferred_deviceGPU*A
forward_function_name(&__forward_gpu_lstm_with_fallback_78710*
go_backwards( *

time_major( :- )
'
_output_shapes
:         :2.
,
_output_shapes
:         Я:-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :2.
,
_output_shapes
:Я         : 

_output_shapes
::1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :	

_output_shapes
::2
.
,
_output_shapes
:Я         :1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :!

_output_shapes	
:З: 

_output_shapes
::-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: : 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
::

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
у
Ы
,__inference_sequential_3_layer_call_fn_75797
lstm_3_input
unknown:<
	unknown_0:<
	unknown_1:<
	unknown_2:
	unknown_3:
identityѕбStatefulPartitionedCallї
StatefulPartitionedCallStatefulPartitionedCalllstm_3_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:         *'
_read_only_resource_inputs	
*0
config_proto 

CPU

GPU2*0J 8ѓ *P
fKRI
G__inference_sequential_3_layer_call_and_return_conditional_losses_75784o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         `
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*5
_input_shapes$
":         Я: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:Z V
,
_output_shapes
:         Я
&
_user_specified_namelstm_3_input
╔Z
є
!__inference__traced_restore_79360
file_prefix1
assignvariableop_dense_3_kernel:-
assignvariableop_1_dense_3_bias:&
assignvariableop_2_adam_iter:	 (
assignvariableop_3_adam_beta_1: (
assignvariableop_4_adam_beta_2: '
assignvariableop_5_adam_decay: /
%assignvariableop_6_adam_learning_rate: >
,assignvariableop_7_lstm_3_lstm_cell_3_kernel:<H
6assignvariableop_8_lstm_3_lstm_cell_3_recurrent_kernel:<8
*assignvariableop_9_lstm_3_lstm_cell_3_bias:<#
assignvariableop_10_total: #
assignvariableop_11_count: ;
)assignvariableop_12_adam_dense_3_kernel_m:5
'assignvariableop_13_adam_dense_3_bias_m:F
4assignvariableop_14_adam_lstm_3_lstm_cell_3_kernel_m:<P
>assignvariableop_15_adam_lstm_3_lstm_cell_3_recurrent_kernel_m:<@
2assignvariableop_16_adam_lstm_3_lstm_cell_3_bias_m:<;
)assignvariableop_17_adam_dense_3_kernel_v:5
'assignvariableop_18_adam_dense_3_bias_v:F
4assignvariableop_19_adam_lstm_3_lstm_cell_3_kernel_v:<P
>assignvariableop_20_adam_lstm_3_lstm_cell_3_recurrent_kernel_v:<@
2assignvariableop_21_adam_lstm_3_lstm_cell_3_bias_v:<
identity_23ѕбAssignVariableOpбAssignVariableOp_1бAssignVariableOp_10бAssignVariableOp_11бAssignVariableOp_12бAssignVariableOp_13бAssignVariableOp_14бAssignVariableOp_15бAssignVariableOp_16бAssignVariableOp_17бAssignVariableOp_18бAssignVariableOp_19бAssignVariableOp_2бAssignVariableOp_20бAssignVariableOp_21бAssignVariableOp_3бAssignVariableOp_4бAssignVariableOp_5бAssignVariableOp_6бAssignVariableOp_7бAssignVariableOp_8бAssignVariableOp_9е
RestoreV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*╬

value─
B┴
B6layer_with_weights-1/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-1/bias/.ATTRIBUTES/VARIABLE_VALUEB)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB&variables/0/.ATTRIBUTES/VARIABLE_VALUEB&variables/1/.ATTRIBUTES/VARIABLE_VALUEB&variables/2/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/0/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/1/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/2/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/0/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/1/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/2/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPHъ
RestoreV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*A
value8B6B B B B B B B B B B B B B B B B B B B B B B B Љ
	RestoreV2	RestoreV2file_prefixRestoreV2/tensor_names:output:0#RestoreV2/shape_and_slices:output:0"/device:CPU:0*p
_output_shapes^
\:::::::::::::::::::::::*%
dtypes
2	[
IdentityIdentityRestoreV2:tensors:0"/device:CPU:0*
T0*
_output_shapes
:і
AssignVariableOpAssignVariableOpassignvariableop_dense_3_kernelIdentity:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_1IdentityRestoreV2:tensors:1"/device:CPU:0*
T0*
_output_shapes
:ј
AssignVariableOp_1AssignVariableOpassignvariableop_1_dense_3_biasIdentity_1:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_2IdentityRestoreV2:tensors:2"/device:CPU:0*
T0	*
_output_shapes
:І
AssignVariableOp_2AssignVariableOpassignvariableop_2_adam_iterIdentity_2:output:0"/device:CPU:0*
_output_shapes
 *
dtype0	]

Identity_3IdentityRestoreV2:tensors:3"/device:CPU:0*
T0*
_output_shapes
:Ї
AssignVariableOp_3AssignVariableOpassignvariableop_3_adam_beta_1Identity_3:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_4IdentityRestoreV2:tensors:4"/device:CPU:0*
T0*
_output_shapes
:Ї
AssignVariableOp_4AssignVariableOpassignvariableop_4_adam_beta_2Identity_4:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_5IdentityRestoreV2:tensors:5"/device:CPU:0*
T0*
_output_shapes
:ї
AssignVariableOp_5AssignVariableOpassignvariableop_5_adam_decayIdentity_5:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_6IdentityRestoreV2:tensors:6"/device:CPU:0*
T0*
_output_shapes
:ћ
AssignVariableOp_6AssignVariableOp%assignvariableop_6_adam_learning_rateIdentity_6:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_7IdentityRestoreV2:tensors:7"/device:CPU:0*
T0*
_output_shapes
:Џ
AssignVariableOp_7AssignVariableOp,assignvariableop_7_lstm_3_lstm_cell_3_kernelIdentity_7:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_8IdentityRestoreV2:tensors:8"/device:CPU:0*
T0*
_output_shapes
:Ц
AssignVariableOp_8AssignVariableOp6assignvariableop_8_lstm_3_lstm_cell_3_recurrent_kernelIdentity_8:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_9IdentityRestoreV2:tensors:9"/device:CPU:0*
T0*
_output_shapes
:Ў
AssignVariableOp_9AssignVariableOp*assignvariableop_9_lstm_3_lstm_cell_3_biasIdentity_9:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_10IdentityRestoreV2:tensors:10"/device:CPU:0*
T0*
_output_shapes
:і
AssignVariableOp_10AssignVariableOpassignvariableop_10_totalIdentity_10:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_11IdentityRestoreV2:tensors:11"/device:CPU:0*
T0*
_output_shapes
:і
AssignVariableOp_11AssignVariableOpassignvariableop_11_countIdentity_11:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_12IdentityRestoreV2:tensors:12"/device:CPU:0*
T0*
_output_shapes
:џ
AssignVariableOp_12AssignVariableOp)assignvariableop_12_adam_dense_3_kernel_mIdentity_12:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_13IdentityRestoreV2:tensors:13"/device:CPU:0*
T0*
_output_shapes
:ў
AssignVariableOp_13AssignVariableOp'assignvariableop_13_adam_dense_3_bias_mIdentity_13:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_14IdentityRestoreV2:tensors:14"/device:CPU:0*
T0*
_output_shapes
:Ц
AssignVariableOp_14AssignVariableOp4assignvariableop_14_adam_lstm_3_lstm_cell_3_kernel_mIdentity_14:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_15IdentityRestoreV2:tensors:15"/device:CPU:0*
T0*
_output_shapes
:»
AssignVariableOp_15AssignVariableOp>assignvariableop_15_adam_lstm_3_lstm_cell_3_recurrent_kernel_mIdentity_15:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_16IdentityRestoreV2:tensors:16"/device:CPU:0*
T0*
_output_shapes
:Б
AssignVariableOp_16AssignVariableOp2assignvariableop_16_adam_lstm_3_lstm_cell_3_bias_mIdentity_16:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_17IdentityRestoreV2:tensors:17"/device:CPU:0*
T0*
_output_shapes
:џ
AssignVariableOp_17AssignVariableOp)assignvariableop_17_adam_dense_3_kernel_vIdentity_17:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_18IdentityRestoreV2:tensors:18"/device:CPU:0*
T0*
_output_shapes
:ў
AssignVariableOp_18AssignVariableOp'assignvariableop_18_adam_dense_3_bias_vIdentity_18:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_19IdentityRestoreV2:tensors:19"/device:CPU:0*
T0*
_output_shapes
:Ц
AssignVariableOp_19AssignVariableOp4assignvariableop_19_adam_lstm_3_lstm_cell_3_kernel_vIdentity_19:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_20IdentityRestoreV2:tensors:20"/device:CPU:0*
T0*
_output_shapes
:»
AssignVariableOp_20AssignVariableOp>assignvariableop_20_adam_lstm_3_lstm_cell_3_recurrent_kernel_vIdentity_20:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_21IdentityRestoreV2:tensors:21"/device:CPU:0*
T0*
_output_shapes
:Б
AssignVariableOp_21AssignVariableOp2assignvariableop_21_adam_lstm_3_lstm_cell_3_bias_vIdentity_21:output:0"/device:CPU:0*
_output_shapes
 *
dtype01
NoOpNoOp"/device:CPU:0*
_output_shapes
 │
Identity_22Identityfile_prefix^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_17^AssignVariableOp_18^AssignVariableOp_19^AssignVariableOp_2^AssignVariableOp_20^AssignVariableOp_21^AssignVariableOp_3^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9^NoOp"/device:CPU:0*
T0*
_output_shapes
: W
Identity_23IdentityIdentity_22:output:0^NoOp_1*
T0*
_output_shapes
: а
NoOp_1NoOp^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_17^AssignVariableOp_18^AssignVariableOp_19^AssignVariableOp_2^AssignVariableOp_20^AssignVariableOp_21^AssignVariableOp_3^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9*"
_acd_function_control_output(*
_output_shapes
 "#
identity_23Identity_23:output:0*A
_input_shapes0
.: : : : : : : : : : : : : : : : : : : : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12*
AssignVariableOp_10AssignVariableOp_102*
AssignVariableOp_11AssignVariableOp_112*
AssignVariableOp_12AssignVariableOp_122*
AssignVariableOp_13AssignVariableOp_132*
AssignVariableOp_14AssignVariableOp_142*
AssignVariableOp_15AssignVariableOp_152*
AssignVariableOp_16AssignVariableOp_162*
AssignVariableOp_17AssignVariableOp_172*
AssignVariableOp_18AssignVariableOp_182*
AssignVariableOp_19AssignVariableOp_192(
AssignVariableOp_2AssignVariableOp_22*
AssignVariableOp_20AssignVariableOp_202*
AssignVariableOp_21AssignVariableOp_212(
AssignVariableOp_3AssignVariableOp_32(
AssignVariableOp_4AssignVariableOp_42(
AssignVariableOp_5AssignVariableOp_52(
AssignVariableOp_6AssignVariableOp_62(
AssignVariableOp_7AssignVariableOp_72(
AssignVariableOp_8AssignVariableOp_82(
AssignVariableOp_9AssignVariableOp_9:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix
к(
╬
while_body_78355
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0
while_matmul_kernel_0%
!while_matmul_1_recurrent_kernel_0
while_biasadd_bias_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor
while_matmul_kernel#
while_matmul_1_recurrent_kernel
while_biasadd_biasѕ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       д
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:         *
element_dtype0Љ
while/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0while_matmul_kernel_0*
T0*'
_output_shapes
:         <ѓ
while/MatMul_1MatMulwhile_placeholder_2!while_matmul_1_recurrent_kernel_0*
T0*'
_output_shapes
:         <v
	while/addAddV2while/MatMul:product:0while/MatMul_1:product:0*
T0*'
_output_shapes
:         <o
while/BiasAddBiasAddwhile/add:z:0while_biasadd_bias_0*
T0*'
_output_shapes
:         <W
while/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :╚
while/splitSplitwhile/split/split_dim:output:0while/BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_split`
while/SigmoidSigmoidwhile/split:output:0*
T0*'
_output_shapes
:         b
while/Sigmoid_1Sigmoidwhile/split:output:1*
T0*'
_output_shapes
:         l
	while/mulMulwhile/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:         Z

while/TanhTanhwhile/split:output:2*
T0*'
_output_shapes
:         g
while/mul_1Mulwhile/Sigmoid:y:0while/Tanh:y:0*
T0*'
_output_shapes
:         f
while/add_1AddV2while/mul:z:0while/mul_1:z:0*
T0*'
_output_shapes
:         b
while/Sigmoid_2Sigmoidwhile/split:output:3*
T0*'
_output_shapes
:         W
while/Tanh_1Tanhwhile/add_1:z:0*
T0*'
_output_shapes
:         k
while/mul_2Mulwhile/Sigmoid_2:y:0while/Tanh_1:y:0*
T0*'
_output_shapes
:         И
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/mul_2:z:0*
_output_shapes
: *
element_dtype0:жУмO
while/add_2/yConst*
_output_shapes
: *
dtype0*
value	B :`
while/add_2AddV2while_placeholderwhile/add_2/y:output:0*
T0*
_output_shapes
: O
while/add_3/yConst*
_output_shapes
: *
dtype0*
value	B :g
while/add_3AddV2while_while_loop_counterwhile/add_3/y:output:0*
T0*
_output_shapes
: L
while/IdentityIdentitywhile/add_3:z:0*
T0*
_output_shapes
: ]
while/Identity_1Identitywhile_while_maximum_iterations*
T0*
_output_shapes
: N
while/Identity_2Identitywhile/add_2:z:0*
T0*
_output_shapes
: ї
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: :жУм_
while/Identity_4Identitywhile/mul_2:z:0*
T0*'
_output_shapes
:         _
while/Identity_5Identitywhile/add_1:z:0*
T0*'
_output_shapes
:         "*
while_biasadd_biaswhile_biasadd_bias_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"D
while_matmul_1_recurrent_kernel!while_matmul_1_recurrent_kernel_0",
while_matmul_kernelwhile_matmul_kernel_0",
while_strided_slicewhile_strided_slice_0"е
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*_
_input_shapesN
L: : : : :         :         : : :<:<:<: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:<:$	 

_output_shapes

:<: 


_output_shapes
:<
к(
╬
while_body_76963
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0
while_matmul_kernel_0%
!while_matmul_1_recurrent_kernel_0
while_biasadd_bias_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor
while_matmul_kernel#
while_matmul_1_recurrent_kernel
while_biasadd_biasѕ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       д
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:         *
element_dtype0Љ
while/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0while_matmul_kernel_0*
T0*'
_output_shapes
:         <ѓ
while/MatMul_1MatMulwhile_placeholder_2!while_matmul_1_recurrent_kernel_0*
T0*'
_output_shapes
:         <v
	while/addAddV2while/MatMul:product:0while/MatMul_1:product:0*
T0*'
_output_shapes
:         <o
while/BiasAddBiasAddwhile/add:z:0while_biasadd_bias_0*
T0*'
_output_shapes
:         <W
while/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :╚
while/splitSplitwhile/split/split_dim:output:0while/BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_split`
while/SigmoidSigmoidwhile/split:output:0*
T0*'
_output_shapes
:         b
while/Sigmoid_1Sigmoidwhile/split:output:1*
T0*'
_output_shapes
:         l
	while/mulMulwhile/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:         Z

while/TanhTanhwhile/split:output:2*
T0*'
_output_shapes
:         g
while/mul_1Mulwhile/Sigmoid:y:0while/Tanh:y:0*
T0*'
_output_shapes
:         f
while/add_1AddV2while/mul:z:0while/mul_1:z:0*
T0*'
_output_shapes
:         b
while/Sigmoid_2Sigmoidwhile/split:output:3*
T0*'
_output_shapes
:         W
while/Tanh_1Tanhwhile/add_1:z:0*
T0*'
_output_shapes
:         k
while/mul_2Mulwhile/Sigmoid_2:y:0while/Tanh_1:y:0*
T0*'
_output_shapes
:         И
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/mul_2:z:0*
_output_shapes
: *
element_dtype0:жУмO
while/add_2/yConst*
_output_shapes
: *
dtype0*
value	B :`
while/add_2AddV2while_placeholderwhile/add_2/y:output:0*
T0*
_output_shapes
: O
while/add_3/yConst*
_output_shapes
: *
dtype0*
value	B :g
while/add_3AddV2while_while_loop_counterwhile/add_3/y:output:0*
T0*
_output_shapes
: L
while/IdentityIdentitywhile/add_3:z:0*
T0*
_output_shapes
: ]
while/Identity_1Identitywhile_while_maximum_iterations*
T0*
_output_shapes
: N
while/Identity_2Identitywhile/add_2:z:0*
T0*
_output_shapes
: ї
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: :жУм_
while/Identity_4Identitywhile/mul_2:z:0*
T0*'
_output_shapes
:         _
while/Identity_5Identitywhile/add_1:z:0*
T0*'
_output_shapes
:         "*
while_biasadd_biaswhile_biasadd_bias_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"D
while_matmul_1_recurrent_kernel!while_matmul_1_recurrent_kernel_0",
while_matmul_kernelwhile_matmul_kernel_0",
while_strided_slicewhile_strided_slice_0"е
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*_
_input_shapesN
L: : : : :         :         : : :<:<:<: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:<:$	 

_output_shapes

:<: 


_output_shapes
:<
§K
Б
&__forward_gpu_lstm_with_fallback_77318

inputs
init_h_0
init_c_0

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4
cudnnrnn
transpose_9_perm

cudnnrnn_0

cudnnrnn_1

cudnnrnn_2
	transpose

expanddims
expanddims_1
concat_1
transpose_perm

init_h

init_c
concat_1_axis
transpose_1_perm
transpose_2_perm
transpose_3_perm
transpose_4_perm
transpose_5_perm
transpose_6_perm
transpose_7_perm
transpose_8_perm
split_2_split_dim
split_split_dim
split_1_split_dim
concat_axisѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          B
transpose_0	Transposeinputstranspose/perm:output:0*
T0P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : q

ExpandDims
ExpandDimsinit_h_0ExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : u
ExpandDims_1
ExpandDimsinit_c_0ExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : ё

concat_1_0ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0¤
CudnnRNNCudnnRNNtranspose_0:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1_0:output:0*
T0*^
_output_shapesL
J:Я         :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          }
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*,
_output_shapes
:         Яp
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_9:y:0*
T0*,
_output_shapes
:         ЯZ

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
concat_1concat_1_0:output:0"'
concat_1_axisconcat_1/axis:output:0"#
concat_axisconcat/axis:output:0"
cudnnrnnCudnnRNN:output:0"!

cudnnrnn_0CudnnRNN:output_h:0"!

cudnnrnn_1CudnnRNN:output_c:0"&

cudnnrnn_2CudnnRNN:reserve_space:0"!

expanddimsExpandDims:output:0"%
expanddims_1ExpandDims_1:output:0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"
init_cinit_c_0"
init_hinit_h_0"/
split_1_split_dimsplit_1/split_dim:output:0"/
split_2_split_dimsplit_2/split_dim:output:0"+
split_split_dimsplit/split_dim:output:0"
	transposetranspose_0:y:0"-
transpose_1_permtranspose_1/perm:output:0"-
transpose_2_permtranspose_2/perm:output:0"-
transpose_3_permtranspose_3/perm:output:0"-
transpose_4_permtranspose_4/perm:output:0"-
transpose_5_permtranspose_5/perm:output:0"-
transpose_6_permtranspose_6/perm:output:0"-
transpose_7_permtranspose_7/perm:output:0"-
transpose_8_permtranspose_8/perm:output:0"-
transpose_9_permtranspose_9/perm:output:0")
transpose_permtranspose/perm:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_91ae4c3a-1bb4-4840-80b7-ab1926c68132*
api_preferred_deviceGPU*U
backward_function_name;9__inference___backward_gpu_lstm_with_fallback_77143_77319*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
▓
├
G__inference_sequential_3_layer_call_and_return_conditional_losses_75784

inputs
lstm_3_75760:<
lstm_3_75762:<
lstm_3_75764:<
dense_3_75778:
dense_3_75780:
identityѕбdense_3/StatefulPartitionedCallбlstm_3/StatefulPartitionedCallЭ
lstm_3/StatefulPartitionedCallStatefulPartitionedCallinputslstm_3_75760lstm_3_75762lstm_3_75764*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:         *%
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8ѓ *J
fERC
A__inference_lstm_3_layer_call_and_return_conditional_losses_75759Ї
dense_3/StatefulPartitionedCallStatefulPartitionedCall'lstm_3/StatefulPartitionedCall:output:0dense_3_75778dense_3_75780*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:         *$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8ѓ *K
fFRD
B__inference_dense_3_layer_call_and_return_conditional_losses_75777w
IdentityIdentity(dense_3/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         Ѕ
NoOpNoOp ^dense_3/StatefulPartitionedCall^lstm_3/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*5
_input_shapes$
":         Я: : : : : 2B
dense_3/StatefulPartitionedCalldense_3/StatefulPartitionedCall2@
lstm_3/StatefulPartitionedCalllstm_3/StatefulPartitionedCall:T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs
§K
Б
&__forward_gpu_lstm_with_fallback_76849

inputs
init_h_0
init_c_0

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4
cudnnrnn
transpose_9_perm

cudnnrnn_0

cudnnrnn_1

cudnnrnn_2
	transpose

expanddims
expanddims_1
concat_1
transpose_perm

init_h

init_c
concat_1_axis
transpose_1_perm
transpose_2_perm
transpose_3_perm
transpose_4_perm
transpose_5_perm
transpose_6_perm
transpose_7_perm
transpose_8_perm
split_2_split_dim
split_split_dim
split_1_split_dim
concat_axisѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          B
transpose_0	Transposeinputstranspose/perm:output:0*
T0P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : q

ExpandDims
ExpandDimsinit_h_0ExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : u
ExpandDims_1
ExpandDimsinit_c_0ExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : ё

concat_1_0ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0¤
CudnnRNNCudnnRNNtranspose_0:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1_0:output:0*
T0*^
_output_shapesL
J:Я         :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          }
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*,
_output_shapes
:         Яp
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_9:y:0*
T0*,
_output_shapes
:         ЯZ

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
concat_1concat_1_0:output:0"'
concat_1_axisconcat_1/axis:output:0"#
concat_axisconcat/axis:output:0"
cudnnrnnCudnnRNN:output:0"!

cudnnrnn_0CudnnRNN:output_h:0"!

cudnnrnn_1CudnnRNN:output_c:0"&

cudnnrnn_2CudnnRNN:reserve_space:0"!

expanddimsExpandDims:output:0"%
expanddims_1ExpandDims_1:output:0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"
init_cinit_c_0"
init_hinit_h_0"/
split_1_split_dimsplit_1/split_dim:output:0"/
split_2_split_dimsplit_2/split_dim:output:0"+
split_split_dimsplit/split_dim:output:0"
	transposetranspose_0:y:0"-
transpose_1_permtranspose_1/perm:output:0"-
transpose_2_permtranspose_2/perm:output:0"-
transpose_3_permtranspose_3/perm:output:0"-
transpose_4_permtranspose_4/perm:output:0"-
transpose_5_permtranspose_5/perm:output:0"-
transpose_6_permtranspose_6/perm:output:0"-
transpose_7_permtranspose_7/perm:output:0"-
transpose_8_permtranspose_8/perm:output:0"-
transpose_9_permtranspose_9/perm:output:0")
transpose_permtranspose/perm:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_a92946bc-3bab-4632-a518-695c88a7828f*
api_preferred_deviceGPU*U
backward_function_name;9__inference___backward_gpu_lstm_with_fallback_76674_76850*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
к(
╬
while_body_75401
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0
while_matmul_kernel_0%
!while_matmul_1_recurrent_kernel_0
while_biasadd_bias_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor
while_matmul_kernel#
while_matmul_1_recurrent_kernel
while_biasadd_biasѕ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       д
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:         *
element_dtype0Љ
while/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0while_matmul_kernel_0*
T0*'
_output_shapes
:         <ѓ
while/MatMul_1MatMulwhile_placeholder_2!while_matmul_1_recurrent_kernel_0*
T0*'
_output_shapes
:         <v
	while/addAddV2while/MatMul:product:0while/MatMul_1:product:0*
T0*'
_output_shapes
:         <o
while/BiasAddBiasAddwhile/add:z:0while_biasadd_bias_0*
T0*'
_output_shapes
:         <W
while/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :╚
while/splitSplitwhile/split/split_dim:output:0while/BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_split`
while/SigmoidSigmoidwhile/split:output:0*
T0*'
_output_shapes
:         b
while/Sigmoid_1Sigmoidwhile/split:output:1*
T0*'
_output_shapes
:         l
	while/mulMulwhile/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:         Z

while/TanhTanhwhile/split:output:2*
T0*'
_output_shapes
:         g
while/mul_1Mulwhile/Sigmoid:y:0while/Tanh:y:0*
T0*'
_output_shapes
:         f
while/add_1AddV2while/mul:z:0while/mul_1:z:0*
T0*'
_output_shapes
:         b
while/Sigmoid_2Sigmoidwhile/split:output:3*
T0*'
_output_shapes
:         W
while/Tanh_1Tanhwhile/add_1:z:0*
T0*'
_output_shapes
:         k
while/mul_2Mulwhile/Sigmoid_2:y:0while/Tanh_1:y:0*
T0*'
_output_shapes
:         И
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/mul_2:z:0*
_output_shapes
: *
element_dtype0:жУмO
while/add_2/yConst*
_output_shapes
: *
dtype0*
value	B :`
while/add_2AddV2while_placeholderwhile/add_2/y:output:0*
T0*
_output_shapes
: O
while/add_3/yConst*
_output_shapes
: *
dtype0*
value	B :g
while/add_3AddV2while_while_loop_counterwhile/add_3/y:output:0*
T0*
_output_shapes
: L
while/IdentityIdentitywhile/add_3:z:0*
T0*
_output_shapes
: ]
while/Identity_1Identitywhile_while_maximum_iterations*
T0*
_output_shapes
: N
while/Identity_2Identitywhile/add_2:z:0*
T0*
_output_shapes
: ї
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: :жУм_
while/Identity_4Identitywhile/mul_2:z:0*
T0*'
_output_shapes
:         _
while/Identity_5Identitywhile/add_1:z:0*
T0*'
_output_shapes
:         "*
while_biasadd_biaswhile_biasadd_bias_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"D
while_matmul_1_recurrent_kernel!while_matmul_1_recurrent_kernel_0",
while_matmul_kernelwhile_matmul_kernel_0",
while_strided_slicewhile_strided_slice_0"е
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*_
_input_shapesN
L: : : : :         :         : : :<:<:<: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:<:$	 

_output_shapes

:<: 


_output_shapes
:<
§K
Б
&__forward_gpu_lstm_with_fallback_74395

inputs
init_h_0
init_c_0

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4
cudnnrnn
transpose_9_perm

cudnnrnn_0

cudnnrnn_1

cudnnrnn_2
	transpose

expanddims
expanddims_1
concat_1
transpose_perm

init_h

init_c
concat_1_axis
transpose_1_perm
transpose_2_perm
transpose_3_perm
transpose_4_perm
transpose_5_perm
transpose_6_perm
transpose_7_perm
transpose_8_perm
split_2_split_dim
split_split_dim
split_1_split_dim
concat_axisѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          B
transpose_0	Transposeinputstranspose/perm:output:0*
T0P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : q

ExpandDims
ExpandDimsinit_h_0ExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : u
ExpandDims_1
ExpandDimsinit_c_0ExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : ё

concat_1_0ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0¤
CudnnRNNCudnnRNNtranspose_0:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1_0:output:0*
T0*^
_output_shapesL
J:Я         :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          }
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*,
_output_shapes
:         Яp
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_9:y:0*
T0*,
_output_shapes
:         ЯZ

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
concat_1concat_1_0:output:0"'
concat_1_axisconcat_1/axis:output:0"#
concat_axisconcat/axis:output:0"
cudnnrnnCudnnRNN:output:0"!

cudnnrnn_0CudnnRNN:output_h:0"!

cudnnrnn_1CudnnRNN:output_c:0"&

cudnnrnn_2CudnnRNN:reserve_space:0"!

expanddimsExpandDims:output:0"%
expanddims_1ExpandDims_1:output:0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"
init_cinit_c_0"
init_hinit_h_0"/
split_1_split_dimsplit_1/split_dim:output:0"/
split_2_split_dimsplit_2/split_dim:output:0"+
split_split_dimsplit/split_dim:output:0"
	transposetranspose_0:y:0"-
transpose_1_permtranspose_1/perm:output:0"-
transpose_2_permtranspose_2/perm:output:0"-
transpose_3_permtranspose_3/perm:output:0"-
transpose_4_permtranspose_4/perm:output:0"-
transpose_5_permtranspose_5/perm:output:0"-
transpose_6_permtranspose_6/perm:output:0"-
transpose_7_permtranspose_7/perm:output:0"-
transpose_8_permtranspose_8/perm:output:0"-
transpose_9_permtranspose_9/perm:output:0")
transpose_permtranspose/perm:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_0067de53-6b05-422e-aca0-48144f07387a*
api_preferred_deviceGPU*U
backward_function_name;9__inference___backward_gpu_lstm_with_fallback_74220_74396*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
Х<
И
A__inference_lstm_3_layer_call_and_return_conditional_losses_75313

inputs.
read_readvariableop_resource:<0
read_1_readvariableop_resource:<,
read_2_readvariableop_resource:<

identity_3ѕбRead/ReadVariableOpбRead_1/ReadVariableOpбRead_2/ReadVariableOp;
ShapeShapeinputs*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Л
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskP
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :s
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:P
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    l
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:         R
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :w
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:R
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    r
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:         E
ones_like/ShapeShapeinputs*
T0*
_output_shapes
:T
ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ђ?ё
	ones_likeFillones_like/Shape:output:0ones_like/Const:output:0*
T0*4
_output_shapes"
 :                  R
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?}
dropout/MulMulones_like:output:0dropout/Const:output:0*
T0*4
_output_shapes"
 :                  O
dropout/ShapeShapeones_like:output:0*
T0*
_output_shapes
:Ў
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*4
_output_shapes"
 :                  *
dtype0[
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<│
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*4
_output_shapes"
 :                  |
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*4
_output_shapes"
 :                  v
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*4
_output_shapes"
 :                  T
dropout_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?Ђ
dropout_1/MulMulones_like:output:0dropout_1/Const:output:0*
T0*4
_output_shapes"
 :                  Q
dropout_1/ShapeShapeones_like:output:0*
T0*
_output_shapes
:Ю
&dropout_1/random_uniform/RandomUniformRandomUniformdropout_1/Shape:output:0*
T0*4
_output_shapes"
 :                  *
dtype0]
dropout_1/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<╣
dropout_1/GreaterEqualGreaterEqual/dropout_1/random_uniform/RandomUniform:output:0!dropout_1/GreaterEqual/y:output:0*
T0*4
_output_shapes"
 :                  ђ
dropout_1/CastCastdropout_1/GreaterEqual:z:0*

DstT0*

SrcT0
*4
_output_shapes"
 :                  |
dropout_1/Mul_1Muldropout_1/Mul:z:0dropout_1/Cast:y:0*
T0*4
_output_shapes"
 :                  T
dropout_2/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?Ђ
dropout_2/MulMulones_like:output:0dropout_2/Const:output:0*
T0*4
_output_shapes"
 :                  Q
dropout_2/ShapeShapeones_like:output:0*
T0*
_output_shapes
:Ю
&dropout_2/random_uniform/RandomUniformRandomUniformdropout_2/Shape:output:0*
T0*4
_output_shapes"
 :                  *
dtype0]
dropout_2/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<╣
dropout_2/GreaterEqualGreaterEqual/dropout_2/random_uniform/RandomUniform:output:0!dropout_2/GreaterEqual/y:output:0*
T0*4
_output_shapes"
 :                  ђ
dropout_2/CastCastdropout_2/GreaterEqual:z:0*

DstT0*

SrcT0
*4
_output_shapes"
 :                  |
dropout_2/Mul_1Muldropout_2/Mul:z:0dropout_2/Cast:y:0*
T0*4
_output_shapes"
 :                  T
dropout_3/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?Ђ
dropout_3/MulMulones_like:output:0dropout_3/Const:output:0*
T0*4
_output_shapes"
 :                  Q
dropout_3/ShapeShapeones_like:output:0*
T0*
_output_shapes
:Ю
&dropout_3/random_uniform/RandomUniformRandomUniformdropout_3/Shape:output:0*
T0*4
_output_shapes"
 :                  *
dtype0]
dropout_3/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<╣
dropout_3/GreaterEqualGreaterEqual/dropout_3/random_uniform/RandomUniform:output:0!dropout_3/GreaterEqual/y:output:0*
T0*4
_output_shapes"
 :                  ђ
dropout_3/CastCastdropout_3/GreaterEqual:z:0*

DstT0*

SrcT0
*4
_output_shapes"
 :                  |
dropout_3/Mul_1Muldropout_3/Mul:z:0dropout_3/Cast:y:0*
T0*4
_output_shapes"
 :                  d
mulMulinputsdropout/Mul_1:z:0*
T0*4
_output_shapes"
 :                  p
Read/ReadVariableOpReadVariableOpread_readvariableop_resource*
_output_shapes

:<*
dtype0Z
IdentityIdentityRead/ReadVariableOp:value:0*
T0*
_output_shapes

:<t
Read_1/ReadVariableOpReadVariableOpread_1_readvariableop_resource*
_output_shapes

:<*
dtype0^

Identity_1IdentityRead_1/ReadVariableOp:value:0*
T0*
_output_shapes

:<p
Read_2/ReadVariableOpReadVariableOpread_2_readvariableop_resource*
_output_shapes
:<*
dtype0Z

Identity_2IdentityRead_2/ReadVariableOp:value:0*
T0*
_output_shapes
:<Й
PartitionedCallPartitionedCallmul:z:0zeros:output:0zeros_1:output:0Identity:output:0Identity_1:output:0Identity_2:output:0*
Tin

2*
Tout	
2*
_collective_manager_ids
 *o
_output_shapes]
[:         :                  :         :         : * 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8ѓ *(
f#R!
__inference_standard_lstm_75040i

Identity_3IdentityPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         ї
NoOpNoOp^Read/ReadVariableOp^Read_1/ReadVariableOp^Read_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:                  : : : 2*
Read/ReadVariableOpRead/ReadVariableOp2.
Read_1/ReadVariableOpRead_1/ReadVariableOp2.
Read_2/ReadVariableOpRead_2/ReadVariableOp:\ X
4
_output_shapes"
 :                  
 
_user_specified_nameinputs
к(
╬
while_body_75925
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0
while_matmul_kernel_0%
!while_matmul_1_recurrent_kernel_0
while_biasadd_bias_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor
while_matmul_kernel#
while_matmul_1_recurrent_kernel
while_biasadd_biasѕ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       д
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:         *
element_dtype0Љ
while/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0while_matmul_kernel_0*
T0*'
_output_shapes
:         <ѓ
while/MatMul_1MatMulwhile_placeholder_2!while_matmul_1_recurrent_kernel_0*
T0*'
_output_shapes
:         <v
	while/addAddV2while/MatMul:product:0while/MatMul_1:product:0*
T0*'
_output_shapes
:         <o
while/BiasAddBiasAddwhile/add:z:0while_biasadd_bias_0*
T0*'
_output_shapes
:         <W
while/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :╚
while/splitSplitwhile/split/split_dim:output:0while/BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_split`
while/SigmoidSigmoidwhile/split:output:0*
T0*'
_output_shapes
:         b
while/Sigmoid_1Sigmoidwhile/split:output:1*
T0*'
_output_shapes
:         l
	while/mulMulwhile/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:         Z

while/TanhTanhwhile/split:output:2*
T0*'
_output_shapes
:         g
while/mul_1Mulwhile/Sigmoid:y:0while/Tanh:y:0*
T0*'
_output_shapes
:         f
while/add_1AddV2while/mul:z:0while/mul_1:z:0*
T0*'
_output_shapes
:         b
while/Sigmoid_2Sigmoidwhile/split:output:3*
T0*'
_output_shapes
:         W
while/Tanh_1Tanhwhile/add_1:z:0*
T0*'
_output_shapes
:         k
while/mul_2Mulwhile/Sigmoid_2:y:0while/Tanh_1:y:0*
T0*'
_output_shapes
:         И
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/mul_2:z:0*
_output_shapes
: *
element_dtype0:жУмO
while/add_2/yConst*
_output_shapes
: *
dtype0*
value	B :`
while/add_2AddV2while_placeholderwhile/add_2/y:output:0*
T0*
_output_shapes
: O
while/add_3/yConst*
_output_shapes
: *
dtype0*
value	B :g
while/add_3AddV2while_while_loop_counterwhile/add_3/y:output:0*
T0*
_output_shapes
: L
while/IdentityIdentitywhile/add_3:z:0*
T0*
_output_shapes
: ]
while/Identity_1Identitywhile_while_maximum_iterations*
T0*
_output_shapes
: N
while/Identity_2Identitywhile/add_2:z:0*
T0*
_output_shapes
: ї
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: :жУм_
while/Identity_4Identitywhile/mul_2:z:0*
T0*'
_output_shapes
:         _
while/Identity_5Identitywhile/add_1:z:0*
T0*'
_output_shapes
:         "*
while_biasadd_biaswhile_biasadd_bias_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"D
while_matmul_1_recurrent_kernel!while_matmul_1_recurrent_kernel_0",
while_matmul_kernelwhile_matmul_kernel_0",
while_strided_slicewhile_strided_slice_0"е
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*_
_input_shapesN
L: : : : :         :         : : :<:<:<: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:<:$	 

_output_shapes

:<: 


_output_shapes
:<
Є(
ў
G__inference_sequential_3_layer_call_and_return_conditional_losses_76858

inputs5
#lstm_3_read_readvariableop_resource:<7
%lstm_3_read_1_readvariableop_resource:<3
%lstm_3_read_2_readvariableop_resource:<8
&dense_3_matmul_readvariableop_resource:5
'dense_3_biasadd_readvariableop_resource:
identityѕбdense_3/BiasAdd/ReadVariableOpбdense_3/MatMul/ReadVariableOpбlstm_3/Read/ReadVariableOpбlstm_3/Read_1/ReadVariableOpбlstm_3/Read_2/ReadVariableOpB
lstm_3/ShapeShapeinputs*
T0*
_output_shapes
:d
lstm_3/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: f
lstm_3/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:f
lstm_3/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:З
lstm_3/strided_sliceStridedSlicelstm_3/Shape:output:0#lstm_3/strided_slice/stack:output:0%lstm_3/strided_slice/stack_1:output:0%lstm_3/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskW
lstm_3/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :ѕ
lstm_3/zeros/packedPacklstm_3/strided_slice:output:0lstm_3/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:W
lstm_3/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    Ђ
lstm_3/zerosFilllstm_3/zeros/packed:output:0lstm_3/zeros/Const:output:0*
T0*'
_output_shapes
:         Y
lstm_3/zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :ї
lstm_3/zeros_1/packedPacklstm_3/strided_slice:output:0 lstm_3/zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:Y
lstm_3/zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    Є
lstm_3/zeros_1Filllstm_3/zeros_1/packed:output:0lstm_3/zeros_1/Const:output:0*
T0*'
_output_shapes
:         L
lstm_3/ones_like/ShapeShapeinputs*
T0*
_output_shapes
:[
lstm_3/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ђ?Љ
lstm_3/ones_likeFilllstm_3/ones_like/Shape:output:0lstm_3/ones_like/Const:output:0*
T0*,
_output_shapes
:         Яk

lstm_3/mulMulinputslstm_3/ones_like:output:0*
T0*,
_output_shapes
:         Я~
lstm_3/Read/ReadVariableOpReadVariableOp#lstm_3_read_readvariableop_resource*
_output_shapes

:<*
dtype0h
lstm_3/IdentityIdentity"lstm_3/Read/ReadVariableOp:value:0*
T0*
_output_shapes

:<ѓ
lstm_3/Read_1/ReadVariableOpReadVariableOp%lstm_3_read_1_readvariableop_resource*
_output_shapes

:<*
dtype0l
lstm_3/Identity_1Identity$lstm_3/Read_1/ReadVariableOp:value:0*
T0*
_output_shapes

:<~
lstm_3/Read_2/ReadVariableOpReadVariableOp%lstm_3_read_2_readvariableop_resource*
_output_shapes
:<*
dtype0h
lstm_3/Identity_2Identity$lstm_3/Read_2/ReadVariableOp:value:0*
T0*
_output_shapes
:<у
lstm_3/PartitionedCallPartitionedCalllstm_3/mul:z:0lstm_3/zeros:output:0lstm_3/zeros_1:output:0lstm_3/Identity:output:0lstm_3/Identity_1:output:0lstm_3/Identity_2:output:0*
Tin

2*
Tout	
2*
_collective_manager_ids
 *g
_output_shapesU
S:         :         Я:         :         : * 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8ѓ *(
f#R!
__inference_standard_lstm_76579ё
dense_3/MatMul/ReadVariableOpReadVariableOp&dense_3_matmul_readvariableop_resource*
_output_shapes

:*
dtype0њ
dense_3/MatMulMatMullstm_3/PartitionedCall:output:0%dense_3/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:         ѓ
dense_3/BiasAdd/ReadVariableOpReadVariableOp'dense_3_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0ј
dense_3/BiasAddBiasAdddense_3/MatMul:product:0&dense_3/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:         g
IdentityIdentitydense_3/BiasAdd:output:0^NoOp*
T0*'
_output_shapes
:         Р
NoOpNoOp^dense_3/BiasAdd/ReadVariableOp^dense_3/MatMul/ReadVariableOp^lstm_3/Read/ReadVariableOp^lstm_3/Read_1/ReadVariableOp^lstm_3/Read_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*5
_input_shapes$
":         Я: : : : : 2@
dense_3/BiasAdd/ReadVariableOpdense_3/BiasAdd/ReadVariableOp2>
dense_3/MatMul/ReadVariableOpdense_3/MatMul/ReadVariableOp28
lstm_3/Read/ReadVariableOplstm_3/Read/ReadVariableOp2<
lstm_3/Read_1/ReadVariableOplstm_3/Read_1/ReadVariableOp2<
lstm_3/Read_2/ReadVariableOplstm_3/Read_2/ReadVariableOp:T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs
╠I
ў
G__inference_sequential_3_layer_call_and_return_conditional_losses_77327

inputs5
#lstm_3_read_readvariableop_resource:<7
%lstm_3_read_1_readvariableop_resource:<3
%lstm_3_read_2_readvariableop_resource:<8
&dense_3_matmul_readvariableop_resource:5
'dense_3_biasadd_readvariableop_resource:
identityѕбdense_3/BiasAdd/ReadVariableOpбdense_3/MatMul/ReadVariableOpбlstm_3/Read/ReadVariableOpбlstm_3/Read_1/ReadVariableOpбlstm_3/Read_2/ReadVariableOpB
lstm_3/ShapeShapeinputs*
T0*
_output_shapes
:d
lstm_3/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: f
lstm_3/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:f
lstm_3/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:З
lstm_3/strided_sliceStridedSlicelstm_3/Shape:output:0#lstm_3/strided_slice/stack:output:0%lstm_3/strided_slice/stack_1:output:0%lstm_3/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskW
lstm_3/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :ѕ
lstm_3/zeros/packedPacklstm_3/strided_slice:output:0lstm_3/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:W
lstm_3/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    Ђ
lstm_3/zerosFilllstm_3/zeros/packed:output:0lstm_3/zeros/Const:output:0*
T0*'
_output_shapes
:         Y
lstm_3/zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :ї
lstm_3/zeros_1/packedPacklstm_3/strided_slice:output:0 lstm_3/zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:Y
lstm_3/zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    Є
lstm_3/zeros_1Filllstm_3/zeros_1/packed:output:0lstm_3/zeros_1/Const:output:0*
T0*'
_output_shapes
:         L
lstm_3/ones_like/ShapeShapeinputs*
T0*
_output_shapes
:[
lstm_3/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ђ?Љ
lstm_3/ones_likeFilllstm_3/ones_like/Shape:output:0lstm_3/ones_like/Const:output:0*
T0*,
_output_shapes
:         ЯY
lstm_3/dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?і
lstm_3/dropout/MulMullstm_3/ones_like:output:0lstm_3/dropout/Const:output:0*
T0*,
_output_shapes
:         Я]
lstm_3/dropout/ShapeShapelstm_3/ones_like:output:0*
T0*
_output_shapes
:Ъ
+lstm_3/dropout/random_uniform/RandomUniformRandomUniformlstm_3/dropout/Shape:output:0*
T0*,
_output_shapes
:         Я*
dtype0b
lstm_3/dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<└
lstm_3/dropout/GreaterEqualGreaterEqual4lstm_3/dropout/random_uniform/RandomUniform:output:0&lstm_3/dropout/GreaterEqual/y:output:0*
T0*,
_output_shapes
:         Яѓ
lstm_3/dropout/CastCastlstm_3/dropout/GreaterEqual:z:0*

DstT0*

SrcT0
*,
_output_shapes
:         ЯЃ
lstm_3/dropout/Mul_1Mullstm_3/dropout/Mul:z:0lstm_3/dropout/Cast:y:0*
T0*,
_output_shapes
:         Я[
lstm_3/dropout_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?ј
lstm_3/dropout_1/MulMullstm_3/ones_like:output:0lstm_3/dropout_1/Const:output:0*
T0*,
_output_shapes
:         Я_
lstm_3/dropout_1/ShapeShapelstm_3/ones_like:output:0*
T0*
_output_shapes
:Б
-lstm_3/dropout_1/random_uniform/RandomUniformRandomUniformlstm_3/dropout_1/Shape:output:0*
T0*,
_output_shapes
:         Я*
dtype0d
lstm_3/dropout_1/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<к
lstm_3/dropout_1/GreaterEqualGreaterEqual6lstm_3/dropout_1/random_uniform/RandomUniform:output:0(lstm_3/dropout_1/GreaterEqual/y:output:0*
T0*,
_output_shapes
:         Яє
lstm_3/dropout_1/CastCast!lstm_3/dropout_1/GreaterEqual:z:0*

DstT0*

SrcT0
*,
_output_shapes
:         ЯЅ
lstm_3/dropout_1/Mul_1Mullstm_3/dropout_1/Mul:z:0lstm_3/dropout_1/Cast:y:0*
T0*,
_output_shapes
:         Я[
lstm_3/dropout_2/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?ј
lstm_3/dropout_2/MulMullstm_3/ones_like:output:0lstm_3/dropout_2/Const:output:0*
T0*,
_output_shapes
:         Я_
lstm_3/dropout_2/ShapeShapelstm_3/ones_like:output:0*
T0*
_output_shapes
:Б
-lstm_3/dropout_2/random_uniform/RandomUniformRandomUniformlstm_3/dropout_2/Shape:output:0*
T0*,
_output_shapes
:         Я*
dtype0d
lstm_3/dropout_2/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<к
lstm_3/dropout_2/GreaterEqualGreaterEqual6lstm_3/dropout_2/random_uniform/RandomUniform:output:0(lstm_3/dropout_2/GreaterEqual/y:output:0*
T0*,
_output_shapes
:         Яє
lstm_3/dropout_2/CastCast!lstm_3/dropout_2/GreaterEqual:z:0*

DstT0*

SrcT0
*,
_output_shapes
:         ЯЅ
lstm_3/dropout_2/Mul_1Mullstm_3/dropout_2/Mul:z:0lstm_3/dropout_2/Cast:y:0*
T0*,
_output_shapes
:         Я[
lstm_3/dropout_3/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?ј
lstm_3/dropout_3/MulMullstm_3/ones_like:output:0lstm_3/dropout_3/Const:output:0*
T0*,
_output_shapes
:         Я_
lstm_3/dropout_3/ShapeShapelstm_3/ones_like:output:0*
T0*
_output_shapes
:Б
-lstm_3/dropout_3/random_uniform/RandomUniformRandomUniformlstm_3/dropout_3/Shape:output:0*
T0*,
_output_shapes
:         Я*
dtype0d
lstm_3/dropout_3/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<к
lstm_3/dropout_3/GreaterEqualGreaterEqual6lstm_3/dropout_3/random_uniform/RandomUniform:output:0(lstm_3/dropout_3/GreaterEqual/y:output:0*
T0*,
_output_shapes
:         Яє
lstm_3/dropout_3/CastCast!lstm_3/dropout_3/GreaterEqual:z:0*

DstT0*

SrcT0
*,
_output_shapes
:         ЯЅ
lstm_3/dropout_3/Mul_1Mullstm_3/dropout_3/Mul:z:0lstm_3/dropout_3/Cast:y:0*
T0*,
_output_shapes
:         Яj

lstm_3/mulMulinputslstm_3/dropout/Mul_1:z:0*
T0*,
_output_shapes
:         Я~
lstm_3/Read/ReadVariableOpReadVariableOp#lstm_3_read_readvariableop_resource*
_output_shapes

:<*
dtype0h
lstm_3/IdentityIdentity"lstm_3/Read/ReadVariableOp:value:0*
T0*
_output_shapes

:<ѓ
lstm_3/Read_1/ReadVariableOpReadVariableOp%lstm_3_read_1_readvariableop_resource*
_output_shapes

:<*
dtype0l
lstm_3/Identity_1Identity$lstm_3/Read_1/ReadVariableOp:value:0*
T0*
_output_shapes

:<~
lstm_3/Read_2/ReadVariableOpReadVariableOp%lstm_3_read_2_readvariableop_resource*
_output_shapes
:<*
dtype0h
lstm_3/Identity_2Identity$lstm_3/Read_2/ReadVariableOp:value:0*
T0*
_output_shapes
:<у
lstm_3/PartitionedCallPartitionedCalllstm_3/mul:z:0lstm_3/zeros:output:0lstm_3/zeros_1:output:0lstm_3/Identity:output:0lstm_3/Identity_1:output:0lstm_3/Identity_2:output:0*
Tin

2*
Tout	
2*
_collective_manager_ids
 *g
_output_shapesU
S:         :         Я:         :         : * 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8ѓ *(
f#R!
__inference_standard_lstm_77048ё
dense_3/MatMul/ReadVariableOpReadVariableOp&dense_3_matmul_readvariableop_resource*
_output_shapes

:*
dtype0њ
dense_3/MatMulMatMullstm_3/PartitionedCall:output:0%dense_3/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:         ѓ
dense_3/BiasAdd/ReadVariableOpReadVariableOp'dense_3_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0ј
dense_3/BiasAddBiasAdddense_3/MatMul:product:0&dense_3/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:         g
IdentityIdentitydense_3/BiasAdd:output:0^NoOp*
T0*'
_output_shapes
:         Р
NoOpNoOp^dense_3/BiasAdd/ReadVariableOp^dense_3/MatMul/ReadVariableOp^lstm_3/Read/ReadVariableOp^lstm_3/Read_1/ReadVariableOp^lstm_3/Read_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*5
_input_shapes$
":         Я: : : : : 2@
dense_3/BiasAdd/ReadVariableOpdense_3/BiasAdd/ReadVariableOp2>
dense_3/MatMul/ReadVariableOpdense_3/MatMul/ReadVariableOp28
lstm_3/Read/ReadVariableOplstm_3/Read/ReadVariableOp2<
lstm_3/Read_1/ReadVariableOplstm_3/Read_1/ReadVariableOp2<
lstm_3/Read_2/ReadVariableOplstm_3/Read_2/ReadVariableOp:T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs
ї	
╝
while_cond_77923
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_77923___redundant_placeholder03
/while_while_cond_77923___redundant_placeholder13
/while_while_cond_77923___redundant_placeholder23
/while_while_cond_77923___redundant_placeholder3
while_identity
`

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: K
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: ")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*S
_input_shapesB
@: : : : :         :         : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
::

_output_shapes
::	

_output_shapes
::


_output_shapes
:
Њ:
┐
__inference_standard_lstm_78903

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4c
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          n
	transpose	Transposeinputstranspose/perm:output:0*
T0*,
_output_shapes
:Я         B
ShapeShapetranspose:y:0*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Л
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskf
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
         ▓
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмє
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Я
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУм_
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:ж
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maskd
MatMulMatMulstrided_slice_1:output:0kernel*
T0*'
_output_shapes
:         <^
MatMul_1MatMulinit_hrecurrent_kernel*
T0*'
_output_shapes
:         <d
addAddV2MatMul:product:0MatMul_1:product:0*
T0*'
_output_shapes
:         <S
BiasAddBiasAddadd:z:0bias*
T0*'
_output_shapes
:         <Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :Х
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_splitT
SigmoidSigmoidsplit:output:0*
T0*'
_output_shapes
:         V
	Sigmoid_1Sigmoidsplit:output:1*
T0*'
_output_shapes
:         S
mulMulSigmoid_1:y:0init_c*
T0*'
_output_shapes
:         N
TanhTanhsplit:output:2*
T0*'
_output_shapes
:         U
mul_1MulSigmoid:y:0Tanh:y:0*
T0*'
_output_shapes
:         T
add_1AddV2mul:z:0	mul_1:z:0*
T0*'
_output_shapes
:         V
	Sigmoid_2Sigmoidsplit:output:3*
T0*'
_output_shapes
:         K
Tanh_1Tanh	add_1:z:0*
T0*'
_output_shapes
:         Y
mul_2MulSigmoid_2:y:0
Tanh_1:y:0*
T0*'
_output_shapes
:         n
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Х
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмF
timeConst*
_output_shapes
: *
dtype0*
value	B : c
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
         T
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : «
whileStatelessWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0init_hinit_cstrided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0kernelrecurrent_kernelbias*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*`
_output_shapesN
L: : : : :         :         : : :<:<:<* 
_read_only_resource_inputs
 *
_stateful_parallelism( *
bodyR
while_body_78818*
condR
while_cond_78817*_
output_shapesN
L: : : : :         :         : : :<:<:<*
parallel_iterations Ђ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       ├
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*,
_output_shapes
:Я         *
element_dtype0h
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
         a
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Є
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          Ќ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*,
_output_shapes
:         Я[
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *  ђ?`
IdentityIdentitystrided_slice_2:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_1:y:0*
T0*,
_output_shapes
:         ЯX

Identity_2Identitywhile:output:4*
T0*'
_output_shapes
:         X

Identity_3Identitywhile:output:5*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_428e9d17-ff82-4115-bdf7-a5882938e408*
api_preferred_deviceCPU*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
а@
╦
(__inference_gpu_lstm_with_fallback_75580

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4ѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          n
	transpose	Transposeinputstranspose/perm:output:0*
T0*,
_output_shapes
:Я         P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : o

ExpandDims
ExpandDimsinit_hExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : s
ExpandDims_1
ExpandDimsinit_cExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : Ъ
concat_1ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0*
_output_shapes	
:З╦
CudnnRNNCudnnRNNtranspose:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1:output:0*
T0*^
_output_shapesL
J:Я         :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          }
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*,
_output_shapes
:         Яp
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_9:y:0*
T0*,
_output_shapes
:         ЯZ

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_d78c62b6-1045-479a-bb0d-b6d9b57a4016*
api_preferred_deviceGPU*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
м┬
ы
9__inference___backward_gpu_lstm_with_fallback_78104_78280
placeholder
placeholder_1
placeholder_2
placeholder_3
placeholder_4/
+gradients_strided_slice_grad_shape_cudnnrnnA
=gradients_transpose_9_grad_invertpermutation_transpose_9_perm)
%gradients_squeeze_grad_shape_cudnnrnn+
'gradients_squeeze_1_grad_shape_cudnnrnn!
gradients_zeros_like_cudnnrnn6
2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose7
3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims9
5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_15
1gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1=
9gradients_transpose_grad_invertpermutation_transpose_perm*
&gradients_expanddims_grad_shape_init_h,
(gradients_expanddims_1_grad_shape_init_c-
)gradients_concat_1_grad_mod_concat_1_axisA
=gradients_transpose_1_grad_invertpermutation_transpose_1_permA
=gradients_transpose_2_grad_invertpermutation_transpose_2_permA
=gradients_transpose_3_grad_invertpermutation_transpose_3_permA
=gradients_transpose_4_grad_invertpermutation_transpose_4_permA
=gradients_transpose_5_grad_invertpermutation_transpose_5_permA
=gradients_transpose_6_grad_invertpermutation_transpose_6_permA
=gradients_transpose_7_grad_invertpermutation_transpose_7_permA
=gradients_transpose_8_grad_invertpermutation_transpose_8_perm3
/gradients_split_2_grad_concat_split_2_split_dim/
+gradients_split_grad_concat_split_split_dim3
/gradients_split_1_grad_concat_split_1_split_dim)
%gradients_concat_grad_mod_concat_axis
identity

identity_1

identity_2

identity_3

identity_4

identity_5ѕ^
gradients/grad_ys_0Identityplaceholder*
T0*'
_output_shapes
:         m
gradients/grad_ys_1Identityplaceholder_1*
T0*4
_output_shapes"
 :                  `
gradients/grad_ys_2Identityplaceholder_2*
T0*'
_output_shapes
:         `
gradients/grad_ys_3Identityplaceholder_3*
T0*'
_output_shapes
:         O
gradients/grad_ys_4Identityplaceholder_4*
T0*
_output_shapes
: }
"gradients/strided_slice_grad/ShapeShape+gradients_strided_slice_grad_shape_cudnnrnn*
T0*
_output_shapes
:є
3gradients/strided_slice_grad/StridedSliceGrad/beginConst*
_output_shapes
:*
dtype0*
valueB:
         {
1gradients/strided_slice_grad/StridedSliceGrad/endConst*
_output_shapes
:*
dtype0*
valueB: 
5gradients/strided_slice_grad/StridedSliceGrad/stridesConst*
_output_shapes
:*
dtype0*
valueB:ф
-gradients/strided_slice_grad/StridedSliceGradStridedSliceGrad+gradients/strided_slice_grad/Shape:output:0<gradients/strided_slice_grad/StridedSliceGrad/begin:output:0:gradients/strided_slice_grad/StridedSliceGrad/end:output:0>gradients/strided_slice_grad/StridedSliceGrad/strides:output:0gradients/grad_ys_0:output:0*
Index0*
T0*4
_output_shapes"
 :                  *
shrink_axis_maskю
,gradients/transpose_9_grad/InvertPermutationInvertPermutation=gradients_transpose_9_grad_invertpermutation_transpose_9_perm*
_output_shapes
:└
$gradients/transpose_9_grad/transpose	Transposegradients/grad_ys_1:output:00gradients/transpose_9_grad/InvertPermutation:y:0*
T0*4
_output_shapes"
 :                  q
gradients/Squeeze_grad/ShapeShape%gradients_squeeze_grad_shape_cudnnrnn*
T0*
_output_shapes
:ц
gradients/Squeeze_grad/ReshapeReshapegradients/grad_ys_2:output:0%gradients/Squeeze_grad/Shape:output:0*
T0*+
_output_shapes
:         u
gradients/Squeeze_1_grad/ShapeShape'gradients_squeeze_1_grad_shape_cudnnrnn*
T0*
_output_shapes
:е
 gradients/Squeeze_1_grad/ReshapeReshapegradients/grad_ys_3:output:0'gradients/Squeeze_1_grad/Shape:output:0*
T0*+
_output_shapes
:         ѓ
gradients/AddNAddN6gradients/strided_slice_grad/StridedSliceGrad:output:0(gradients/transpose_9_grad/transpose:y:0*
N*
T0*@
_class6
42loc:@gradients/strided_slice_grad/StridedSliceGrad*4
_output_shapes"
 :                  c
gradients/zeros_like	ZerosLikegradients_zeros_like_cudnnrnn*
T0*
_output_shapes
:Ѕ
(gradients/CudnnRNN_grad/CudnnRNNBackpropCudnnRNNBackprop2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_11gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1+gradients_strided_slice_grad_shape_cudnnrnn%gradients_squeeze_grad_shape_cudnnrnn'gradients_squeeze_1_grad_shape_cudnnrnngradients/AddN:sum:0'gradients/Squeeze_grad/Reshape:output:0)gradients/Squeeze_1_grad/Reshape:output:0gradients_zeros_like_cudnnrnn*
T0*i
_output_shapesW
U:                  :         :         :Зќ
*gradients/transpose_grad/InvertPermutationInvertPermutation9gradients_transpose_grad_invertpermutation_transpose_perm*
_output_shapes
:┘
"gradients/transpose_grad/transpose	Transpose9gradients/CudnnRNN_grad/CudnnRNNBackprop:input_backprop:0.gradients/transpose_grad/InvertPermutation:y:0*
T0*4
_output_shapes"
 :                  u
gradients/ExpandDims_grad/ShapeShape&gradients_expanddims_grad_shape_init_h*
T0*
_output_shapes
:┼
!gradients/ExpandDims_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_h_backprop:0(gradients/ExpandDims_grad/Shape:output:0*
T0*'
_output_shapes
:         y
!gradients/ExpandDims_1_grad/ShapeShape(gradients_expanddims_1_grad_shape_init_c*
T0*
_output_shapes
:╔
#gradients/ExpandDims_1_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_c_backprop:0*gradients/ExpandDims_1_grad/Shape:output:0*
T0*'
_output_shapes
:         ^
gradients/concat_1_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :џ
gradients/concat_1_grad/modFloorMod)gradients_concat_1_grad_mod_concat_1_axis%gradients/concat_1_grad/Rank:output:0*
T0*
_output_shapes
: g
gradients/concat_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_2Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_3Const*
_output_shapes
:*
dtype0*
valueB:j
gradients/concat_1_grad/Shape_4Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_5Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_6Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_7Const*
_output_shapes
:*
dtype0*
valueB:рi
gradients/concat_1_grad/Shape_8Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_9Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_10Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_11Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_12Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_13Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_14Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_15Const*
_output_shapes
:*
dtype0*
valueB:Э
$gradients/concat_1_grad/ConcatOffsetConcatOffsetgradients/concat_1_grad/mod:z:0&gradients/concat_1_grad/Shape:output:0(gradients/concat_1_grad/Shape_1:output:0(gradients/concat_1_grad/Shape_2:output:0(gradients/concat_1_grad/Shape_3:output:0(gradients/concat_1_grad/Shape_4:output:0(gradients/concat_1_grad/Shape_5:output:0(gradients/concat_1_grad/Shape_6:output:0(gradients/concat_1_grad/Shape_7:output:0(gradients/concat_1_grad/Shape_8:output:0(gradients/concat_1_grad/Shape_9:output:0)gradients/concat_1_grad/Shape_10:output:0)gradients/concat_1_grad/Shape_11:output:0)gradients/concat_1_grad/Shape_12:output:0)gradients/concat_1_grad/Shape_13:output:0)gradients/concat_1_grad/Shape_14:output:0)gradients/concat_1_grad/Shape_15:output:0*
N*t
_output_shapesb
`::::::::::::::::в
gradients/concat_1_grad/SliceSlice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:0&gradients/concat_1_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_1Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:1(gradients/concat_1_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_2Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:2(gradients/concat_1_grad/Shape_2:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_3Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:3(gradients/concat_1_grad/Shape_3:output:0*
Index0*
T0*
_output_shapes
:­
gradients/concat_1_grad/Slice_4Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:4(gradients/concat_1_grad/Shape_4:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_5Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:5(gradients/concat_1_grad/Shape_5:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_6Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:6(gradients/concat_1_grad/Shape_6:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_7Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:7(gradients/concat_1_grad/Shape_7:output:0*
Index0*
T0*
_output_shapes	
:р№
gradients/concat_1_grad/Slice_8Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:8(gradients/concat_1_grad/Shape_8:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_9Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:9(gradients/concat_1_grad/Shape_9:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_10Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:10)gradients/concat_1_grad/Shape_10:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_11Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:11)gradients/concat_1_grad/Shape_11:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_12Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:12)gradients/concat_1_grad/Shape_12:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_13Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:13)gradients/concat_1_grad/Shape_13:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_14Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:14)gradients/concat_1_grad/Shape_14:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_15Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:15)gradients/concat_1_grad/Shape_15:output:0*
Index0*
T0*
_output_shapes
:m
gradients/Reshape_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      А
gradients/Reshape_grad/ReshapeReshape&gradients/concat_1_grad/Slice:output:0%gradients/Reshape_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_1_grad/ReshapeReshape(gradients/concat_1_grad/Slice_1:output:0'gradients/Reshape_1_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_2_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_2_grad/ReshapeReshape(gradients/concat_1_grad/Slice_2:output:0'gradients/Reshape_2_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_3_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_3_grad/ReshapeReshape(gradients/concat_1_grad/Slice_3:output:0'gradients/Reshape_3_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_4_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_4_grad/ReshapeReshape(gradients/concat_1_grad/Slice_4:output:0'gradients/Reshape_4_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_5_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_5_grad/ReshapeReshape(gradients/concat_1_grad/Slice_5:output:0'gradients/Reshape_5_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_6_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_6_grad/ReshapeReshape(gradients/concat_1_grad/Slice_6:output:0'gradients/Reshape_6_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_7_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_7_grad/ReshapeReshape(gradients/concat_1_grad/Slice_7:output:0'gradients/Reshape_7_grad/Shape:output:0*
T0*
_output_shapes

:h
gradients/Reshape_8_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_8_grad/ReshapeReshape(gradients/concat_1_grad/Slice_8:output:0'gradients/Reshape_8_grad/Shape:output:0*
T0*
_output_shapes
:h
gradients/Reshape_9_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_9_grad/ReshapeReshape(gradients/concat_1_grad/Slice_9:output:0'gradients/Reshape_9_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_10_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_10_grad/ReshapeReshape)gradients/concat_1_grad/Slice_10:output:0(gradients/Reshape_10_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_11_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_11_grad/ReshapeReshape)gradients/concat_1_grad/Slice_11:output:0(gradients/Reshape_11_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_12_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_12_grad/ReshapeReshape)gradients/concat_1_grad/Slice_12:output:0(gradients/Reshape_12_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_13_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_13_grad/ReshapeReshape)gradients/concat_1_grad/Slice_13:output:0(gradients/Reshape_13_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_14_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_14_grad/ReshapeReshape)gradients/concat_1_grad/Slice_14:output:0(gradients/Reshape_14_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_15_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_15_grad/ReshapeReshape)gradients/concat_1_grad/Slice_15:output:0(gradients/Reshape_15_grad/Shape:output:0*
T0*
_output_shapes
:ю
,gradients/transpose_1_grad/InvertPermutationInvertPermutation=gradients_transpose_1_grad_invertpermutation_transpose_1_perm*
_output_shapes
:х
$gradients/transpose_1_grad/transpose	Transpose'gradients/Reshape_grad/Reshape:output:00gradients/transpose_1_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_2_grad/InvertPermutationInvertPermutation=gradients_transpose_2_grad_invertpermutation_transpose_2_perm*
_output_shapes
:и
$gradients/transpose_2_grad/transpose	Transpose)gradients/Reshape_1_grad/Reshape:output:00gradients/transpose_2_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_3_grad/InvertPermutationInvertPermutation=gradients_transpose_3_grad_invertpermutation_transpose_3_perm*
_output_shapes
:и
$gradients/transpose_3_grad/transpose	Transpose)gradients/Reshape_2_grad/Reshape:output:00gradients/transpose_3_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_4_grad/InvertPermutationInvertPermutation=gradients_transpose_4_grad_invertpermutation_transpose_4_perm*
_output_shapes
:и
$gradients/transpose_4_grad/transpose	Transpose)gradients/Reshape_3_grad/Reshape:output:00gradients/transpose_4_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_5_grad/InvertPermutationInvertPermutation=gradients_transpose_5_grad_invertpermutation_transpose_5_perm*
_output_shapes
:и
$gradients/transpose_5_grad/transpose	Transpose)gradients/Reshape_4_grad/Reshape:output:00gradients/transpose_5_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_6_grad/InvertPermutationInvertPermutation=gradients_transpose_6_grad_invertpermutation_transpose_6_perm*
_output_shapes
:и
$gradients/transpose_6_grad/transpose	Transpose)gradients/Reshape_5_grad/Reshape:output:00gradients/transpose_6_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_7_grad/InvertPermutationInvertPermutation=gradients_transpose_7_grad_invertpermutation_transpose_7_perm*
_output_shapes
:и
$gradients/transpose_7_grad/transpose	Transpose)gradients/Reshape_6_grad/Reshape:output:00gradients/transpose_7_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_8_grad/InvertPermutationInvertPermutation=gradients_transpose_8_grad_invertpermutation_transpose_8_perm*
_output_shapes
:и
$gradients/transpose_8_grad/transpose	Transpose)gradients/Reshape_7_grad/Reshape:output:00gradients/transpose_8_grad/InvertPermutation:y:0*
T0*
_output_shapes

:Т
gradients/split_2_grad/concatConcatV2)gradients/Reshape_8_grad/Reshape:output:0)gradients/Reshape_9_grad/Reshape:output:0*gradients/Reshape_10_grad/Reshape:output:0*gradients/Reshape_11_grad/Reshape:output:0*gradients/Reshape_12_grad/Reshape:output:0*gradients/Reshape_13_grad/Reshape:output:0*gradients/Reshape_14_grad/Reshape:output:0*gradients/Reshape_15_grad/Reshape:output:0/gradients_split_2_grad_concat_split_2_split_dim*
N*
T0*
_output_shapes
:x«
gradients/split_grad/concatConcatV2(gradients/transpose_1_grad/transpose:y:0(gradients/transpose_2_grad/transpose:y:0(gradients/transpose_3_grad/transpose:y:0(gradients/transpose_4_grad/transpose:y:0+gradients_split_grad_concat_split_split_dim*
N*
T0*
_output_shapes

:<┤
gradients/split_1_grad/concatConcatV2(gradients/transpose_5_grad/transpose:y:0(gradients/transpose_6_grad/transpose:y:0(gradients/transpose_7_grad/transpose:y:0(gradients/transpose_8_grad/transpose:y:0/gradients_split_1_grad_concat_split_1_split_dim*
N*
T0*
_output_shapes

:<\
gradients/concat_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :њ
gradients/concat_grad/modFloorMod%gradients_concat_grad_mod_concat_axis#gradients/concat_grad/Rank:output:0*
T0*
_output_shapes
: e
gradients/concat_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:<g
gradients/concat_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:<╩
"gradients/concat_grad/ConcatOffsetConcatOffsetgradients/concat_grad/mod:z:0$gradients/concat_grad/Shape:output:0&gradients/concat_grad/Shape_1:output:0*
N* 
_output_shapes
::Л
gradients/concat_grad/SliceSlice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:0$gradients/concat_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:<Н
gradients/concat_grad/Slice_1Slice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:1&gradients/concat_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:<{
IdentityIdentity&gradients/transpose_grad/transpose:y:0*
T0*4
_output_shapes"
 :                  t

Identity_1Identity*gradients/ExpandDims_grad/Reshape:output:0*
T0*'
_output_shapes
:         v

Identity_2Identity,gradients/ExpandDims_1_grad/Reshape:output:0*
T0*'
_output_shapes
:         e

Identity_3Identity$gradients/split_grad/concat:output:0*
T0*
_output_shapes

:<g

Identity_4Identity&gradients/split_1_grad/concat:output:0*
T0*
_output_shapes

:<c

Identity_5Identity&gradients/concat_grad/Slice_1:output:0*
T0*
_output_shapes
:<"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"!

identity_5Identity_5:output:0*(
_construction_contextkEagerRuntime*Ѓ
_input_shapesы
Ь:         :                  :         :         : :                  ::         :         ::                  :         :         :З::         :         : ::::::::: : : : *=
api_implements+)lstm_bb744fac-7292-4ecb-affd-a726f7708b09*
api_preferred_deviceGPU*A
forward_function_name(&__forward_gpu_lstm_with_fallback_78279*
go_backwards( *

time_major( :- )
'
_output_shapes
:         ::6
4
_output_shapes"
 :                  :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: ::6
4
_output_shapes"
 :                  : 

_output_shapes
::1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :	

_output_shapes
:::
6
4
_output_shapes"
 :                  :1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :!

_output_shapes	
:З: 

_output_shapes
::-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: : 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
::

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
┼	
з
B__inference_dense_3_layer_call_and_return_conditional_losses_75777

inputs0
matmul_readvariableop_resource:-
biasadd_readvariableop_resource:
identityѕбBiasAdd/ReadVariableOpбMatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:*
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:         r
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:         _
IdentityIdentityBiasAdd:output:0^NoOp*
T0*'
_output_shapes
:         w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:         : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:         
 
_user_specified_nameinputs
─
╔
G__inference_sequential_3_layer_call_and_return_conditional_losses_76369
lstm_3_input
lstm_3_76356:<
lstm_3_76358:<
lstm_3_76360:<
dense_3_76363:
dense_3_76365:
identityѕбdense_3/StatefulPartitionedCallбlstm_3/StatefulPartitionedCall■
lstm_3/StatefulPartitionedCallStatefulPartitionedCalllstm_3_inputlstm_3_76356lstm_3_76358lstm_3_76360*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:         *%
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8ѓ *J
fERC
A__inference_lstm_3_layer_call_and_return_conditional_losses_75759Ї
dense_3/StatefulPartitionedCallStatefulPartitionedCall'lstm_3/StatefulPartitionedCall:output:0dense_3_76363dense_3_76365*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:         *$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8ѓ *K
fFRD
B__inference_dense_3_layer_call_and_return_conditional_losses_75777w
IdentityIdentity(dense_3/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         Ѕ
NoOpNoOp ^dense_3/StatefulPartitionedCall^lstm_3/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*5
_input_shapes$
":         Я: : : : : 2B
dense_3/StatefulPartitionedCalldense_3/StatefulPartitionedCall2@
lstm_3/StatefulPartitionedCalllstm_3/StatefulPartitionedCall:Z V
,
_output_shapes
:         Я
&
_user_specified_namelstm_3_input
к(
╬
while_body_76494
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0
while_matmul_kernel_0%
!while_matmul_1_recurrent_kernel_0
while_biasadd_bias_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor
while_matmul_kernel#
while_matmul_1_recurrent_kernel
while_biasadd_biasѕ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       д
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:         *
element_dtype0Љ
while/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0while_matmul_kernel_0*
T0*'
_output_shapes
:         <ѓ
while/MatMul_1MatMulwhile_placeholder_2!while_matmul_1_recurrent_kernel_0*
T0*'
_output_shapes
:         <v
	while/addAddV2while/MatMul:product:0while/MatMul_1:product:0*
T0*'
_output_shapes
:         <o
while/BiasAddBiasAddwhile/add:z:0while_biasadd_bias_0*
T0*'
_output_shapes
:         <W
while/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :╚
while/splitSplitwhile/split/split_dim:output:0while/BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_split`
while/SigmoidSigmoidwhile/split:output:0*
T0*'
_output_shapes
:         b
while/Sigmoid_1Sigmoidwhile/split:output:1*
T0*'
_output_shapes
:         l
	while/mulMulwhile/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:         Z

while/TanhTanhwhile/split:output:2*
T0*'
_output_shapes
:         g
while/mul_1Mulwhile/Sigmoid:y:0while/Tanh:y:0*
T0*'
_output_shapes
:         f
while/add_1AddV2while/mul:z:0while/mul_1:z:0*
T0*'
_output_shapes
:         b
while/Sigmoid_2Sigmoidwhile/split:output:3*
T0*'
_output_shapes
:         W
while/Tanh_1Tanhwhile/add_1:z:0*
T0*'
_output_shapes
:         k
while/mul_2Mulwhile/Sigmoid_2:y:0while/Tanh_1:y:0*
T0*'
_output_shapes
:         И
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/mul_2:z:0*
_output_shapes
: *
element_dtype0:жУмO
while/add_2/yConst*
_output_shapes
: *
dtype0*
value	B :`
while/add_2AddV2while_placeholderwhile/add_2/y:output:0*
T0*
_output_shapes
: O
while/add_3/yConst*
_output_shapes
: *
dtype0*
value	B :g
while/add_3AddV2while_while_loop_counterwhile/add_3/y:output:0*
T0*
_output_shapes
: L
while/IdentityIdentitywhile/add_3:z:0*
T0*
_output_shapes
: ]
while/Identity_1Identitywhile_while_maximum_iterations*
T0*
_output_shapes
: N
while/Identity_2Identitywhile/add_2:z:0*
T0*
_output_shapes
: ї
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: :жУм_
while/Identity_4Identitywhile/mul_2:z:0*
T0*'
_output_shapes
:         _
while/Identity_5Identitywhile/add_1:z:0*
T0*'
_output_shapes
:         "*
while_biasadd_biaswhile_biasadd_bias_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"D
while_matmul_1_recurrent_kernel!while_matmul_1_recurrent_kernel_0",
while_matmul_kernelwhile_matmul_kernel_0",
while_strided_slicewhile_strided_slice_0"е
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*_
_input_shapesN
L: : : : :         :         : : :<:<:<: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:<:$	 

_output_shapes

:<: 


_output_shapes
:<
Н
В
,__inference_sequential_3_layer_call_fn_76406

inputs
unknown:<
	unknown_0:<
	unknown_1:<
	unknown_2:
	unknown_3:
identityѕбStatefulPartitionedCallє
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:         *'
_read_only_resource_inputs	
*0
config_proto 

CPU

GPU2*0J 8ѓ *P
fKRI
G__inference_sequential_3_layer_call_and_return_conditional_losses_75784o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         `
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*5
_input_shapes$
":         Я: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs
─
╔
G__inference_sequential_3_layer_call_and_return_conditional_losses_76385
lstm_3_input
lstm_3_76372:<
lstm_3_76374:<
lstm_3_76376:<
dense_3_76379:
dense_3_76381:
identityѕбdense_3/StatefulPartitionedCallбlstm_3/StatefulPartitionedCall■
lstm_3/StatefulPartitionedCallStatefulPartitionedCalllstm_3_inputlstm_3_76372lstm_3_76374lstm_3_76376*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:         *%
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8ѓ *J
fERC
A__inference_lstm_3_layer_call_and_return_conditional_losses_76283Ї
dense_3/StatefulPartitionedCallStatefulPartitionedCall'lstm_3/StatefulPartitionedCall:output:0dense_3_76379dense_3_76381*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:         *$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8ѓ *K
fFRD
B__inference_dense_3_layer_call_and_return_conditional_losses_75777w
IdentityIdentity(dense_3/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         Ѕ
NoOpNoOp ^dense_3/StatefulPartitionedCall^lstm_3/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*5
_input_shapes$
":         Я: : : : : 2B
dense_3/StatefulPartitionedCalldense_3/StatefulPartitionedCall2@
lstm_3/StatefulPartitionedCalllstm_3/StatefulPartitionedCall:Z V
,
_output_shapes
:         Я
&
_user_specified_namelstm_3_input
дL
Б
&__forward_gpu_lstm_with_fallback_74836

inputs
init_h_0
init_c_0

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4
cudnnrnn
transpose_9_perm

cudnnrnn_0

cudnnrnn_1

cudnnrnn_2
	transpose

expanddims
expanddims_1
concat_1
transpose_perm

init_h

init_c
concat_1_axis
transpose_1_perm
transpose_2_perm
transpose_3_perm
transpose_4_perm
transpose_5_perm
transpose_6_perm
transpose_7_perm
transpose_8_perm
split_2_split_dim
split_split_dim
split_1_split_dim
concat_axisѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          B
transpose_0	Transposeinputstranspose/perm:output:0*
T0P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : q

ExpandDims
ExpandDimsinit_h_0ExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : u
ExpandDims_1
ExpandDimsinit_c_0ExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : ё

concat_1_0ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0О
CudnnRNNCudnnRNNtranspose_0:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1_0:output:0*
T0*f
_output_shapesT
R:                  :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          Ё
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*4
_output_shapes"
 :                  p
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         f

Identity_1Identitytranspose_9:y:0*
T0*4
_output_shapes"
 :                  Z

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
concat_1concat_1_0:output:0"'
concat_1_axisconcat_1/axis:output:0"#
concat_axisconcat/axis:output:0"
cudnnrnnCudnnRNN:output:0"!

cudnnrnn_0CudnnRNN:output_h:0"!

cudnnrnn_1CudnnRNN:output_c:0"&

cudnnrnn_2CudnnRNN:reserve_space:0"!

expanddimsExpandDims:output:0"%
expanddims_1ExpandDims_1:output:0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"
init_cinit_c_0"
init_hinit_h_0"/
split_1_split_dimsplit_1/split_dim:output:0"/
split_2_split_dimsplit_2/split_dim:output:0"+
split_split_dimsplit/split_dim:output:0"
	transposetranspose_0:y:0"-
transpose_1_permtranspose_1/perm:output:0"-
transpose_2_permtranspose_2/perm:output:0"-
transpose_3_permtranspose_3/perm:output:0"-
transpose_4_permtranspose_4/perm:output:0"-
transpose_5_permtranspose_5/perm:output:0"-
transpose_6_permtranspose_6/perm:output:0"-
transpose_7_permtranspose_7/perm:output:0"-
transpose_8_permtranspose_8/perm:output:0"-
transpose_9_permtranspose_9/perm:output:0")
transpose_permtranspose/perm:output:0*(
_construction_contextkEagerRuntime*s
_input_shapesb
`:                  :         :         :<:<:<*=
api_implements+)lstm_8d4f5c85-cfbf-442f-90ec-8d72d94e1b41*
api_preferred_deviceGPU*U
backward_function_name;9__inference___backward_gpu_lstm_with_fallback_74661_74837*
go_backwards( *

time_major( :\ X
4
_output_shapes"
 :                  
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
┼	
з
B__inference_dense_3_layer_call_and_return_conditional_losses_79195

inputs0
matmul_readvariableop_resource:-
biasadd_readvariableop_resource:
identityѕбBiasAdd/ReadVariableOpбMatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:*
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:         r
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:         _
IdentityIdentityBiasAdd:output:0^NoOp*
T0*'
_output_shapes
:         w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:         : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:         
 
_user_specified_nameinputs
└<
║
A__inference_lstm_3_layer_call_and_return_conditional_losses_78282
inputs_0.
read_readvariableop_resource:<0
read_1_readvariableop_resource:<,
read_2_readvariableop_resource:<

identity_3ѕбRead/ReadVariableOpбRead_1/ReadVariableOpбRead_2/ReadVariableOp=
ShapeShapeinputs_0*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Л
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskP
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :s
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:P
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    l
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:         R
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :w
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:R
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    r
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:         G
ones_like/ShapeShapeinputs_0*
T0*
_output_shapes
:T
ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ђ?ё
	ones_likeFillones_like/Shape:output:0ones_like/Const:output:0*
T0*4
_output_shapes"
 :                  R
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?}
dropout/MulMulones_like:output:0dropout/Const:output:0*
T0*4
_output_shapes"
 :                  O
dropout/ShapeShapeones_like:output:0*
T0*
_output_shapes
:Ў
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*4
_output_shapes"
 :                  *
dtype0[
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<│
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*4
_output_shapes"
 :                  |
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*4
_output_shapes"
 :                  v
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*4
_output_shapes"
 :                  T
dropout_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?Ђ
dropout_1/MulMulones_like:output:0dropout_1/Const:output:0*
T0*4
_output_shapes"
 :                  Q
dropout_1/ShapeShapeones_like:output:0*
T0*
_output_shapes
:Ю
&dropout_1/random_uniform/RandomUniformRandomUniformdropout_1/Shape:output:0*
T0*4
_output_shapes"
 :                  *
dtype0]
dropout_1/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<╣
dropout_1/GreaterEqualGreaterEqual/dropout_1/random_uniform/RandomUniform:output:0!dropout_1/GreaterEqual/y:output:0*
T0*4
_output_shapes"
 :                  ђ
dropout_1/CastCastdropout_1/GreaterEqual:z:0*

DstT0*

SrcT0
*4
_output_shapes"
 :                  |
dropout_1/Mul_1Muldropout_1/Mul:z:0dropout_1/Cast:y:0*
T0*4
_output_shapes"
 :                  T
dropout_2/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?Ђ
dropout_2/MulMulones_like:output:0dropout_2/Const:output:0*
T0*4
_output_shapes"
 :                  Q
dropout_2/ShapeShapeones_like:output:0*
T0*
_output_shapes
:Ю
&dropout_2/random_uniform/RandomUniformRandomUniformdropout_2/Shape:output:0*
T0*4
_output_shapes"
 :                  *
dtype0]
dropout_2/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<╣
dropout_2/GreaterEqualGreaterEqual/dropout_2/random_uniform/RandomUniform:output:0!dropout_2/GreaterEqual/y:output:0*
T0*4
_output_shapes"
 :                  ђ
dropout_2/CastCastdropout_2/GreaterEqual:z:0*

DstT0*

SrcT0
*4
_output_shapes"
 :                  |
dropout_2/Mul_1Muldropout_2/Mul:z:0dropout_2/Cast:y:0*
T0*4
_output_shapes"
 :                  T
dropout_3/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?Ђ
dropout_3/MulMulones_like:output:0dropout_3/Const:output:0*
T0*4
_output_shapes"
 :                  Q
dropout_3/ShapeShapeones_like:output:0*
T0*
_output_shapes
:Ю
&dropout_3/random_uniform/RandomUniformRandomUniformdropout_3/Shape:output:0*
T0*4
_output_shapes"
 :                  *
dtype0]
dropout_3/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<╣
dropout_3/GreaterEqualGreaterEqual/dropout_3/random_uniform/RandomUniform:output:0!dropout_3/GreaterEqual/y:output:0*
T0*4
_output_shapes"
 :                  ђ
dropout_3/CastCastdropout_3/GreaterEqual:z:0*

DstT0*

SrcT0
*4
_output_shapes"
 :                  |
dropout_3/Mul_1Muldropout_3/Mul:z:0dropout_3/Cast:y:0*
T0*4
_output_shapes"
 :                  f
mulMulinputs_0dropout/Mul_1:z:0*
T0*4
_output_shapes"
 :                  p
Read/ReadVariableOpReadVariableOpread_readvariableop_resource*
_output_shapes

:<*
dtype0Z
IdentityIdentityRead/ReadVariableOp:value:0*
T0*
_output_shapes

:<t
Read_1/ReadVariableOpReadVariableOpread_1_readvariableop_resource*
_output_shapes

:<*
dtype0^

Identity_1IdentityRead_1/ReadVariableOp:value:0*
T0*
_output_shapes

:<p
Read_2/ReadVariableOpReadVariableOpread_2_readvariableop_resource*
_output_shapes
:<*
dtype0Z

Identity_2IdentityRead_2/ReadVariableOp:value:0*
T0*
_output_shapes
:<Й
PartitionedCallPartitionedCallmul:z:0zeros:output:0zeros_1:output:0Identity:output:0Identity_1:output:0Identity_2:output:0*
Tin

2*
Tout	
2*
_collective_manager_ids
 *o
_output_shapes]
[:         :                  :         :         : * 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8ѓ *(
f#R!
__inference_standard_lstm_78009i

Identity_3IdentityPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         ї
NoOpNoOp^Read/ReadVariableOp^Read_1/ReadVariableOp^Read_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:                  : : : 2*
Read/ReadVariableOpRead/ReadVariableOp2.
Read_1/ReadVariableOpRead_1/ReadVariableOp2.
Read_2/ReadVariableOpRead_2/ReadVariableOp:^ Z
4
_output_shapes"
 :                  
"
_user_specified_name
inputs/0
ї	
╝
while_cond_76962
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_76962___redundant_placeholder03
/while_while_cond_76962___redundant_placeholder13
/while_while_cond_76962___redundant_placeholder23
/while_while_cond_76962___redundant_placeholder3
while_identity
`

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: K
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: ")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*S
_input_shapesB
@: : : : :         :         : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
::

_output_shapes
::	

_output_shapes
::


_output_shapes
:
ї	
╝
while_cond_78354
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_78354___redundant_placeholder03
/while_while_cond_78354___redundant_placeholder13
/while_while_cond_78354___redundant_placeholder23
/while_while_cond_78354___redundant_placeholder3
while_identity
`

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: K
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: ")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*S
_input_shapesB
@: : : : :         :         : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
::

_output_shapes
::	

_output_shapes
::


_output_shapes
:
ї	
╝
while_cond_77460
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_77460___redundant_placeholder03
/while_while_cond_77460___redundant_placeholder13
/while_while_cond_77460___redundant_placeholder23
/while_while_cond_77460___redundant_placeholder3
while_identity
`

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: K
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: ")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*S
_input_shapesB
@: : : : :         :         : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
::

_output_shapes
::	

_output_shapes
::


_output_shapes
:
§K
Б
&__forward_gpu_lstm_with_fallback_76280

inputs
init_h_0
init_c_0

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4
cudnnrnn
transpose_9_perm

cudnnrnn_0

cudnnrnn_1

cudnnrnn_2
	transpose

expanddims
expanddims_1
concat_1
transpose_perm

init_h

init_c
concat_1_axis
transpose_1_perm
transpose_2_perm
transpose_3_perm
transpose_4_perm
transpose_5_perm
transpose_6_perm
transpose_7_perm
transpose_8_perm
split_2_split_dim
split_split_dim
split_1_split_dim
concat_axisѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          B
transpose_0	Transposeinputstranspose/perm:output:0*
T0P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : q

ExpandDims
ExpandDimsinit_h_0ExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : u
ExpandDims_1
ExpandDimsinit_c_0ExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : ё

concat_1_0ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0¤
CudnnRNNCudnnRNNtranspose_0:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1_0:output:0*
T0*^
_output_shapesL
J:Я         :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          }
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*,
_output_shapes
:         Яp
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_9:y:0*
T0*,
_output_shapes
:         ЯZ

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
concat_1concat_1_0:output:0"'
concat_1_axisconcat_1/axis:output:0"#
concat_axisconcat/axis:output:0"
cudnnrnnCudnnRNN:output:0"!

cudnnrnn_0CudnnRNN:output_h:0"!

cudnnrnn_1CudnnRNN:output_c:0"&

cudnnrnn_2CudnnRNN:reserve_space:0"!

expanddimsExpandDims:output:0"%
expanddims_1ExpandDims_1:output:0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"
init_cinit_c_0"
init_hinit_h_0"/
split_1_split_dimsplit_1/split_dim:output:0"/
split_2_split_dimsplit_2/split_dim:output:0"+
split_split_dimsplit/split_dim:output:0"
	transposetranspose_0:y:0"-
transpose_1_permtranspose_1/perm:output:0"-
transpose_2_permtranspose_2/perm:output:0"-
transpose_3_permtranspose_3/perm:output:0"-
transpose_4_permtranspose_4/perm:output:0"-
transpose_5_permtranspose_5/perm:output:0"-
transpose_6_permtranspose_6/perm:output:0"-
transpose_7_permtranspose_7/perm:output:0"-
transpose_8_permtranspose_8/perm:output:0"-
transpose_9_permtranspose_9/perm:output:0")
transpose_permtranspose/perm:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_017ba245-194f-42f6-9988-067e14309e69*
api_preferred_deviceGPU*U
backward_function_name;9__inference___backward_gpu_lstm_with_fallback_76105_76281*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
┴
ћ
'__inference_dense_3_layer_call_fn_79185

inputs
unknown:
	unknown_0:
identityѕбStatefulPartitionedCall┌
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:         *$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8ѓ *K
fFRD
B__inference_dense_3_layer_call_and_return_conditional_losses_75777o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         `
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:         : : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:         
 
_user_specified_nameinputs
а@
╦
(__inference_gpu_lstm_with_fallback_76104

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4ѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          n
	transpose	Transposeinputstranspose/perm:output:0*
T0*,
_output_shapes
:Я         P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : o

ExpandDims
ExpandDimsinit_hExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : s
ExpandDims_1
ExpandDimsinit_cExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : Ъ
concat_1ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0*
_output_shapes	
:З╦
CudnnRNNCudnnRNNtranspose:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1:output:0*
T0*^
_output_shapesL
J:Я         :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          }
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*,
_output_shapes
:         Яp
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_9:y:0*
T0*,
_output_shapes
:         ЯZ

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_017ba245-194f-42f6-9988-067e14309e69*
api_preferred_deviceGPU*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
м┬
ы
9__inference___backward_gpu_lstm_with_fallback_77641_77817
placeholder
placeholder_1
placeholder_2
placeholder_3
placeholder_4/
+gradients_strided_slice_grad_shape_cudnnrnnA
=gradients_transpose_9_grad_invertpermutation_transpose_9_perm)
%gradients_squeeze_grad_shape_cudnnrnn+
'gradients_squeeze_1_grad_shape_cudnnrnn!
gradients_zeros_like_cudnnrnn6
2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose7
3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims9
5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_15
1gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1=
9gradients_transpose_grad_invertpermutation_transpose_perm*
&gradients_expanddims_grad_shape_init_h,
(gradients_expanddims_1_grad_shape_init_c-
)gradients_concat_1_grad_mod_concat_1_axisA
=gradients_transpose_1_grad_invertpermutation_transpose_1_permA
=gradients_transpose_2_grad_invertpermutation_transpose_2_permA
=gradients_transpose_3_grad_invertpermutation_transpose_3_permA
=gradients_transpose_4_grad_invertpermutation_transpose_4_permA
=gradients_transpose_5_grad_invertpermutation_transpose_5_permA
=gradients_transpose_6_grad_invertpermutation_transpose_6_permA
=gradients_transpose_7_grad_invertpermutation_transpose_7_permA
=gradients_transpose_8_grad_invertpermutation_transpose_8_perm3
/gradients_split_2_grad_concat_split_2_split_dim/
+gradients_split_grad_concat_split_split_dim3
/gradients_split_1_grad_concat_split_1_split_dim)
%gradients_concat_grad_mod_concat_axis
identity

identity_1

identity_2

identity_3

identity_4

identity_5ѕ^
gradients/grad_ys_0Identityplaceholder*
T0*'
_output_shapes
:         m
gradients/grad_ys_1Identityplaceholder_1*
T0*4
_output_shapes"
 :                  `
gradients/grad_ys_2Identityplaceholder_2*
T0*'
_output_shapes
:         `
gradients/grad_ys_3Identityplaceholder_3*
T0*'
_output_shapes
:         O
gradients/grad_ys_4Identityplaceholder_4*
T0*
_output_shapes
: }
"gradients/strided_slice_grad/ShapeShape+gradients_strided_slice_grad_shape_cudnnrnn*
T0*
_output_shapes
:є
3gradients/strided_slice_grad/StridedSliceGrad/beginConst*
_output_shapes
:*
dtype0*
valueB:
         {
1gradients/strided_slice_grad/StridedSliceGrad/endConst*
_output_shapes
:*
dtype0*
valueB: 
5gradients/strided_slice_grad/StridedSliceGrad/stridesConst*
_output_shapes
:*
dtype0*
valueB:ф
-gradients/strided_slice_grad/StridedSliceGradStridedSliceGrad+gradients/strided_slice_grad/Shape:output:0<gradients/strided_slice_grad/StridedSliceGrad/begin:output:0:gradients/strided_slice_grad/StridedSliceGrad/end:output:0>gradients/strided_slice_grad/StridedSliceGrad/strides:output:0gradients/grad_ys_0:output:0*
Index0*
T0*4
_output_shapes"
 :                  *
shrink_axis_maskю
,gradients/transpose_9_grad/InvertPermutationInvertPermutation=gradients_transpose_9_grad_invertpermutation_transpose_9_perm*
_output_shapes
:└
$gradients/transpose_9_grad/transpose	Transposegradients/grad_ys_1:output:00gradients/transpose_9_grad/InvertPermutation:y:0*
T0*4
_output_shapes"
 :                  q
gradients/Squeeze_grad/ShapeShape%gradients_squeeze_grad_shape_cudnnrnn*
T0*
_output_shapes
:ц
gradients/Squeeze_grad/ReshapeReshapegradients/grad_ys_2:output:0%gradients/Squeeze_grad/Shape:output:0*
T0*+
_output_shapes
:         u
gradients/Squeeze_1_grad/ShapeShape'gradients_squeeze_1_grad_shape_cudnnrnn*
T0*
_output_shapes
:е
 gradients/Squeeze_1_grad/ReshapeReshapegradients/grad_ys_3:output:0'gradients/Squeeze_1_grad/Shape:output:0*
T0*+
_output_shapes
:         ѓ
gradients/AddNAddN6gradients/strided_slice_grad/StridedSliceGrad:output:0(gradients/transpose_9_grad/transpose:y:0*
N*
T0*@
_class6
42loc:@gradients/strided_slice_grad/StridedSliceGrad*4
_output_shapes"
 :                  c
gradients/zeros_like	ZerosLikegradients_zeros_like_cudnnrnn*
T0*
_output_shapes
:Ѕ
(gradients/CudnnRNN_grad/CudnnRNNBackpropCudnnRNNBackprop2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_11gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1+gradients_strided_slice_grad_shape_cudnnrnn%gradients_squeeze_grad_shape_cudnnrnn'gradients_squeeze_1_grad_shape_cudnnrnngradients/AddN:sum:0'gradients/Squeeze_grad/Reshape:output:0)gradients/Squeeze_1_grad/Reshape:output:0gradients_zeros_like_cudnnrnn*
T0*i
_output_shapesW
U:                  :         :         :Зќ
*gradients/transpose_grad/InvertPermutationInvertPermutation9gradients_transpose_grad_invertpermutation_transpose_perm*
_output_shapes
:┘
"gradients/transpose_grad/transpose	Transpose9gradients/CudnnRNN_grad/CudnnRNNBackprop:input_backprop:0.gradients/transpose_grad/InvertPermutation:y:0*
T0*4
_output_shapes"
 :                  u
gradients/ExpandDims_grad/ShapeShape&gradients_expanddims_grad_shape_init_h*
T0*
_output_shapes
:┼
!gradients/ExpandDims_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_h_backprop:0(gradients/ExpandDims_grad/Shape:output:0*
T0*'
_output_shapes
:         y
!gradients/ExpandDims_1_grad/ShapeShape(gradients_expanddims_1_grad_shape_init_c*
T0*
_output_shapes
:╔
#gradients/ExpandDims_1_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_c_backprop:0*gradients/ExpandDims_1_grad/Shape:output:0*
T0*'
_output_shapes
:         ^
gradients/concat_1_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :џ
gradients/concat_1_grad/modFloorMod)gradients_concat_1_grad_mod_concat_1_axis%gradients/concat_1_grad/Rank:output:0*
T0*
_output_shapes
: g
gradients/concat_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_2Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_3Const*
_output_shapes
:*
dtype0*
valueB:j
gradients/concat_1_grad/Shape_4Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_5Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_6Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_7Const*
_output_shapes
:*
dtype0*
valueB:рi
gradients/concat_1_grad/Shape_8Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_9Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_10Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_11Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_12Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_13Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_14Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_15Const*
_output_shapes
:*
dtype0*
valueB:Э
$gradients/concat_1_grad/ConcatOffsetConcatOffsetgradients/concat_1_grad/mod:z:0&gradients/concat_1_grad/Shape:output:0(gradients/concat_1_grad/Shape_1:output:0(gradients/concat_1_grad/Shape_2:output:0(gradients/concat_1_grad/Shape_3:output:0(gradients/concat_1_grad/Shape_4:output:0(gradients/concat_1_grad/Shape_5:output:0(gradients/concat_1_grad/Shape_6:output:0(gradients/concat_1_grad/Shape_7:output:0(gradients/concat_1_grad/Shape_8:output:0(gradients/concat_1_grad/Shape_9:output:0)gradients/concat_1_grad/Shape_10:output:0)gradients/concat_1_grad/Shape_11:output:0)gradients/concat_1_grad/Shape_12:output:0)gradients/concat_1_grad/Shape_13:output:0)gradients/concat_1_grad/Shape_14:output:0)gradients/concat_1_grad/Shape_15:output:0*
N*t
_output_shapesb
`::::::::::::::::в
gradients/concat_1_grad/SliceSlice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:0&gradients/concat_1_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_1Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:1(gradients/concat_1_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_2Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:2(gradients/concat_1_grad/Shape_2:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_3Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:3(gradients/concat_1_grad/Shape_3:output:0*
Index0*
T0*
_output_shapes
:­
gradients/concat_1_grad/Slice_4Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:4(gradients/concat_1_grad/Shape_4:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_5Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:5(gradients/concat_1_grad/Shape_5:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_6Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:6(gradients/concat_1_grad/Shape_6:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_7Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:7(gradients/concat_1_grad/Shape_7:output:0*
Index0*
T0*
_output_shapes	
:р№
gradients/concat_1_grad/Slice_8Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:8(gradients/concat_1_grad/Shape_8:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_9Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:9(gradients/concat_1_grad/Shape_9:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_10Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:10)gradients/concat_1_grad/Shape_10:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_11Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:11)gradients/concat_1_grad/Shape_11:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_12Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:12)gradients/concat_1_grad/Shape_12:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_13Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:13)gradients/concat_1_grad/Shape_13:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_14Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:14)gradients/concat_1_grad/Shape_14:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_15Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:15)gradients/concat_1_grad/Shape_15:output:0*
Index0*
T0*
_output_shapes
:m
gradients/Reshape_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      А
gradients/Reshape_grad/ReshapeReshape&gradients/concat_1_grad/Slice:output:0%gradients/Reshape_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_1_grad/ReshapeReshape(gradients/concat_1_grad/Slice_1:output:0'gradients/Reshape_1_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_2_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_2_grad/ReshapeReshape(gradients/concat_1_grad/Slice_2:output:0'gradients/Reshape_2_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_3_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_3_grad/ReshapeReshape(gradients/concat_1_grad/Slice_3:output:0'gradients/Reshape_3_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_4_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_4_grad/ReshapeReshape(gradients/concat_1_grad/Slice_4:output:0'gradients/Reshape_4_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_5_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_5_grad/ReshapeReshape(gradients/concat_1_grad/Slice_5:output:0'gradients/Reshape_5_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_6_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_6_grad/ReshapeReshape(gradients/concat_1_grad/Slice_6:output:0'gradients/Reshape_6_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_7_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_7_grad/ReshapeReshape(gradients/concat_1_grad/Slice_7:output:0'gradients/Reshape_7_grad/Shape:output:0*
T0*
_output_shapes

:h
gradients/Reshape_8_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_8_grad/ReshapeReshape(gradients/concat_1_grad/Slice_8:output:0'gradients/Reshape_8_grad/Shape:output:0*
T0*
_output_shapes
:h
gradients/Reshape_9_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_9_grad/ReshapeReshape(gradients/concat_1_grad/Slice_9:output:0'gradients/Reshape_9_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_10_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_10_grad/ReshapeReshape)gradients/concat_1_grad/Slice_10:output:0(gradients/Reshape_10_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_11_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_11_grad/ReshapeReshape)gradients/concat_1_grad/Slice_11:output:0(gradients/Reshape_11_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_12_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_12_grad/ReshapeReshape)gradients/concat_1_grad/Slice_12:output:0(gradients/Reshape_12_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_13_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_13_grad/ReshapeReshape)gradients/concat_1_grad/Slice_13:output:0(gradients/Reshape_13_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_14_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_14_grad/ReshapeReshape)gradients/concat_1_grad/Slice_14:output:0(gradients/Reshape_14_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_15_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_15_grad/ReshapeReshape)gradients/concat_1_grad/Slice_15:output:0(gradients/Reshape_15_grad/Shape:output:0*
T0*
_output_shapes
:ю
,gradients/transpose_1_grad/InvertPermutationInvertPermutation=gradients_transpose_1_grad_invertpermutation_transpose_1_perm*
_output_shapes
:х
$gradients/transpose_1_grad/transpose	Transpose'gradients/Reshape_grad/Reshape:output:00gradients/transpose_1_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_2_grad/InvertPermutationInvertPermutation=gradients_transpose_2_grad_invertpermutation_transpose_2_perm*
_output_shapes
:и
$gradients/transpose_2_grad/transpose	Transpose)gradients/Reshape_1_grad/Reshape:output:00gradients/transpose_2_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_3_grad/InvertPermutationInvertPermutation=gradients_transpose_3_grad_invertpermutation_transpose_3_perm*
_output_shapes
:и
$gradients/transpose_3_grad/transpose	Transpose)gradients/Reshape_2_grad/Reshape:output:00gradients/transpose_3_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_4_grad/InvertPermutationInvertPermutation=gradients_transpose_4_grad_invertpermutation_transpose_4_perm*
_output_shapes
:и
$gradients/transpose_4_grad/transpose	Transpose)gradients/Reshape_3_grad/Reshape:output:00gradients/transpose_4_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_5_grad/InvertPermutationInvertPermutation=gradients_transpose_5_grad_invertpermutation_transpose_5_perm*
_output_shapes
:и
$gradients/transpose_5_grad/transpose	Transpose)gradients/Reshape_4_grad/Reshape:output:00gradients/transpose_5_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_6_grad/InvertPermutationInvertPermutation=gradients_transpose_6_grad_invertpermutation_transpose_6_perm*
_output_shapes
:и
$gradients/transpose_6_grad/transpose	Transpose)gradients/Reshape_5_grad/Reshape:output:00gradients/transpose_6_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_7_grad/InvertPermutationInvertPermutation=gradients_transpose_7_grad_invertpermutation_transpose_7_perm*
_output_shapes
:и
$gradients/transpose_7_grad/transpose	Transpose)gradients/Reshape_6_grad/Reshape:output:00gradients/transpose_7_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_8_grad/InvertPermutationInvertPermutation=gradients_transpose_8_grad_invertpermutation_transpose_8_perm*
_output_shapes
:и
$gradients/transpose_8_grad/transpose	Transpose)gradients/Reshape_7_grad/Reshape:output:00gradients/transpose_8_grad/InvertPermutation:y:0*
T0*
_output_shapes

:Т
gradients/split_2_grad/concatConcatV2)gradients/Reshape_8_grad/Reshape:output:0)gradients/Reshape_9_grad/Reshape:output:0*gradients/Reshape_10_grad/Reshape:output:0*gradients/Reshape_11_grad/Reshape:output:0*gradients/Reshape_12_grad/Reshape:output:0*gradients/Reshape_13_grad/Reshape:output:0*gradients/Reshape_14_grad/Reshape:output:0*gradients/Reshape_15_grad/Reshape:output:0/gradients_split_2_grad_concat_split_2_split_dim*
N*
T0*
_output_shapes
:x«
gradients/split_grad/concatConcatV2(gradients/transpose_1_grad/transpose:y:0(gradients/transpose_2_grad/transpose:y:0(gradients/transpose_3_grad/transpose:y:0(gradients/transpose_4_grad/transpose:y:0+gradients_split_grad_concat_split_split_dim*
N*
T0*
_output_shapes

:<┤
gradients/split_1_grad/concatConcatV2(gradients/transpose_5_grad/transpose:y:0(gradients/transpose_6_grad/transpose:y:0(gradients/transpose_7_grad/transpose:y:0(gradients/transpose_8_grad/transpose:y:0/gradients_split_1_grad_concat_split_1_split_dim*
N*
T0*
_output_shapes

:<\
gradients/concat_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :њ
gradients/concat_grad/modFloorMod%gradients_concat_grad_mod_concat_axis#gradients/concat_grad/Rank:output:0*
T0*
_output_shapes
: e
gradients/concat_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:<g
gradients/concat_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:<╩
"gradients/concat_grad/ConcatOffsetConcatOffsetgradients/concat_grad/mod:z:0$gradients/concat_grad/Shape:output:0&gradients/concat_grad/Shape_1:output:0*
N* 
_output_shapes
::Л
gradients/concat_grad/SliceSlice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:0$gradients/concat_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:<Н
gradients/concat_grad/Slice_1Slice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:1&gradients/concat_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:<{
IdentityIdentity&gradients/transpose_grad/transpose:y:0*
T0*4
_output_shapes"
 :                  t

Identity_1Identity*gradients/ExpandDims_grad/Reshape:output:0*
T0*'
_output_shapes
:         v

Identity_2Identity,gradients/ExpandDims_1_grad/Reshape:output:0*
T0*'
_output_shapes
:         e

Identity_3Identity$gradients/split_grad/concat:output:0*
T0*
_output_shapes

:<g

Identity_4Identity&gradients/split_1_grad/concat:output:0*
T0*
_output_shapes

:<c

Identity_5Identity&gradients/concat_grad/Slice_1:output:0*
T0*
_output_shapes
:<"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"!

identity_5Identity_5:output:0*(
_construction_contextkEagerRuntime*Ѓ
_input_shapesы
Ь:         :                  :         :         : :                  ::         :         ::                  :         :         :З::         :         : ::::::::: : : : *=
api_implements+)lstm_9e12c0bd-db3f-4f45-bd70-4aa03a8d3462*
api_preferred_deviceGPU*A
forward_function_name(&__forward_gpu_lstm_with_fallback_77816*
go_backwards( *

time_major( :- )
'
_output_shapes
:         ::6
4
_output_shapes"
 :                  :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: ::6
4
_output_shapes"
 :                  : 

_output_shapes
::1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :	

_output_shapes
:::
6
4
_output_shapes"
 :                  :1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :!

_output_shapes	
:З: 

_output_shapes
::-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: : 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
::

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
├:
┐
__inference_standard_lstm_78009

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4c
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          v
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :                  B
ShapeShapetranspose:y:0*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Л
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskf
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
         ▓
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмє
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Я
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУм_
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:ж
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maskd
MatMulMatMulstrided_slice_1:output:0kernel*
T0*'
_output_shapes
:         <^
MatMul_1MatMulinit_hrecurrent_kernel*
T0*'
_output_shapes
:         <d
addAddV2MatMul:product:0MatMul_1:product:0*
T0*'
_output_shapes
:         <S
BiasAddBiasAddadd:z:0bias*
T0*'
_output_shapes
:         <Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :Х
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_splitT
SigmoidSigmoidsplit:output:0*
T0*'
_output_shapes
:         V
	Sigmoid_1Sigmoidsplit:output:1*
T0*'
_output_shapes
:         S
mulMulSigmoid_1:y:0init_c*
T0*'
_output_shapes
:         N
TanhTanhsplit:output:2*
T0*'
_output_shapes
:         U
mul_1MulSigmoid:y:0Tanh:y:0*
T0*'
_output_shapes
:         T
add_1AddV2mul:z:0	mul_1:z:0*
T0*'
_output_shapes
:         V
	Sigmoid_2Sigmoidsplit:output:3*
T0*'
_output_shapes
:         K
Tanh_1Tanh	add_1:z:0*
T0*'
_output_shapes
:         Y
mul_2MulSigmoid_2:y:0
Tanh_1:y:0*
T0*'
_output_shapes
:         n
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Х
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмF
timeConst*
_output_shapes
: *
dtype0*
value	B : c
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
         T
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : «
whileStatelessWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0init_hinit_cstrided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0kernelrecurrent_kernelbias*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*`
_output_shapesN
L: : : : :         :         : : :<:<:<* 
_read_only_resource_inputs
 *
_stateful_parallelism( *
bodyR
while_body_77924*
condR
while_cond_77923*_
output_shapesN
L: : : : :         :         : : :<:<:<*
parallel_iterations Ђ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       ╦
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :                  *
element_dtype0h
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
         a
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Є
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          Ъ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :                  [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *  ђ?`
IdentityIdentitystrided_slice_2:output:0*
T0*'
_output_shapes
:         f

Identity_1Identitytranspose_1:y:0*
T0*4
_output_shapes"
 :                  X

Identity_2Identitywhile:output:4*
T0*'
_output_shapes
:         X

Identity_3Identitywhile:output:5*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*s
_input_shapesb
`:                  :         :         :<:<:<*=
api_implements+)lstm_bb744fac-7292-4ecb-affd-a726f7708b09*
api_preferred_deviceCPU*
go_backwards( *

time_major( :\ X
4
_output_shapes"
 :                  
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
§K
Б
&__forward_gpu_lstm_with_fallback_75756

inputs
init_h_0
init_c_0

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4
cudnnrnn
transpose_9_perm

cudnnrnn_0

cudnnrnn_1

cudnnrnn_2
	transpose

expanddims
expanddims_1
concat_1
transpose_perm

init_h

init_c
concat_1_axis
transpose_1_perm
transpose_2_perm
transpose_3_perm
transpose_4_perm
transpose_5_perm
transpose_6_perm
transpose_7_perm
transpose_8_perm
split_2_split_dim
split_split_dim
split_1_split_dim
concat_axisѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          B
transpose_0	Transposeinputstranspose/perm:output:0*
T0P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : q

ExpandDims
ExpandDimsinit_h_0ExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : u
ExpandDims_1
ExpandDimsinit_c_0ExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : ё

concat_1_0ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0¤
CudnnRNNCudnnRNNtranspose_0:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1_0:output:0*
T0*^
_output_shapesL
J:Я         :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          }
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*,
_output_shapes
:         Яp
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_9:y:0*
T0*,
_output_shapes
:         ЯZ

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
concat_1concat_1_0:output:0"'
concat_1_axisconcat_1/axis:output:0"#
concat_axisconcat/axis:output:0"
cudnnrnnCudnnRNN:output:0"!

cudnnrnn_0CudnnRNN:output_h:0"!

cudnnrnn_1CudnnRNN:output_c:0"&

cudnnrnn_2CudnnRNN:reserve_space:0"!

expanddimsExpandDims:output:0"%
expanddims_1ExpandDims_1:output:0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"
init_cinit_c_0"
init_hinit_h_0"/
split_1_split_dimsplit_1/split_dim:output:0"/
split_2_split_dimsplit_2/split_dim:output:0"+
split_split_dimsplit/split_dim:output:0"
	transposetranspose_0:y:0"-
transpose_1_permtranspose_1/perm:output:0"-
transpose_2_permtranspose_2/perm:output:0"-
transpose_3_permtranspose_3/perm:output:0"-
transpose_4_permtranspose_4/perm:output:0"-
transpose_5_permtranspose_5/perm:output:0"-
transpose_6_permtranspose_6/perm:output:0"-
transpose_7_permtranspose_7/perm:output:0"-
transpose_8_permtranspose_8/perm:output:0"-
transpose_9_permtranspose_9/perm:output:0")
transpose_permtranspose/perm:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_d78c62b6-1045-479a-bb0d-b6d9b57a4016*
api_preferred_deviceGPU*U
backward_function_name;9__inference___backward_gpu_lstm_with_fallback_75581_75757*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
м┬
ы
9__inference___backward_gpu_lstm_with_fallback_75135_75311
placeholder
placeholder_1
placeholder_2
placeholder_3
placeholder_4/
+gradients_strided_slice_grad_shape_cudnnrnnA
=gradients_transpose_9_grad_invertpermutation_transpose_9_perm)
%gradients_squeeze_grad_shape_cudnnrnn+
'gradients_squeeze_1_grad_shape_cudnnrnn!
gradients_zeros_like_cudnnrnn6
2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose7
3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims9
5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_15
1gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1=
9gradients_transpose_grad_invertpermutation_transpose_perm*
&gradients_expanddims_grad_shape_init_h,
(gradients_expanddims_1_grad_shape_init_c-
)gradients_concat_1_grad_mod_concat_1_axisA
=gradients_transpose_1_grad_invertpermutation_transpose_1_permA
=gradients_transpose_2_grad_invertpermutation_transpose_2_permA
=gradients_transpose_3_grad_invertpermutation_transpose_3_permA
=gradients_transpose_4_grad_invertpermutation_transpose_4_permA
=gradients_transpose_5_grad_invertpermutation_transpose_5_permA
=gradients_transpose_6_grad_invertpermutation_transpose_6_permA
=gradients_transpose_7_grad_invertpermutation_transpose_7_permA
=gradients_transpose_8_grad_invertpermutation_transpose_8_perm3
/gradients_split_2_grad_concat_split_2_split_dim/
+gradients_split_grad_concat_split_split_dim3
/gradients_split_1_grad_concat_split_1_split_dim)
%gradients_concat_grad_mod_concat_axis
identity

identity_1

identity_2

identity_3

identity_4

identity_5ѕ^
gradients/grad_ys_0Identityplaceholder*
T0*'
_output_shapes
:         m
gradients/grad_ys_1Identityplaceholder_1*
T0*4
_output_shapes"
 :                  `
gradients/grad_ys_2Identityplaceholder_2*
T0*'
_output_shapes
:         `
gradients/grad_ys_3Identityplaceholder_3*
T0*'
_output_shapes
:         O
gradients/grad_ys_4Identityplaceholder_4*
T0*
_output_shapes
: }
"gradients/strided_slice_grad/ShapeShape+gradients_strided_slice_grad_shape_cudnnrnn*
T0*
_output_shapes
:є
3gradients/strided_slice_grad/StridedSliceGrad/beginConst*
_output_shapes
:*
dtype0*
valueB:
         {
1gradients/strided_slice_grad/StridedSliceGrad/endConst*
_output_shapes
:*
dtype0*
valueB: 
5gradients/strided_slice_grad/StridedSliceGrad/stridesConst*
_output_shapes
:*
dtype0*
valueB:ф
-gradients/strided_slice_grad/StridedSliceGradStridedSliceGrad+gradients/strided_slice_grad/Shape:output:0<gradients/strided_slice_grad/StridedSliceGrad/begin:output:0:gradients/strided_slice_grad/StridedSliceGrad/end:output:0>gradients/strided_slice_grad/StridedSliceGrad/strides:output:0gradients/grad_ys_0:output:0*
Index0*
T0*4
_output_shapes"
 :                  *
shrink_axis_maskю
,gradients/transpose_9_grad/InvertPermutationInvertPermutation=gradients_transpose_9_grad_invertpermutation_transpose_9_perm*
_output_shapes
:└
$gradients/transpose_9_grad/transpose	Transposegradients/grad_ys_1:output:00gradients/transpose_9_grad/InvertPermutation:y:0*
T0*4
_output_shapes"
 :                  q
gradients/Squeeze_grad/ShapeShape%gradients_squeeze_grad_shape_cudnnrnn*
T0*
_output_shapes
:ц
gradients/Squeeze_grad/ReshapeReshapegradients/grad_ys_2:output:0%gradients/Squeeze_grad/Shape:output:0*
T0*+
_output_shapes
:         u
gradients/Squeeze_1_grad/ShapeShape'gradients_squeeze_1_grad_shape_cudnnrnn*
T0*
_output_shapes
:е
 gradients/Squeeze_1_grad/ReshapeReshapegradients/grad_ys_3:output:0'gradients/Squeeze_1_grad/Shape:output:0*
T0*+
_output_shapes
:         ѓ
gradients/AddNAddN6gradients/strided_slice_grad/StridedSliceGrad:output:0(gradients/transpose_9_grad/transpose:y:0*
N*
T0*@
_class6
42loc:@gradients/strided_slice_grad/StridedSliceGrad*4
_output_shapes"
 :                  c
gradients/zeros_like	ZerosLikegradients_zeros_like_cudnnrnn*
T0*
_output_shapes
:Ѕ
(gradients/CudnnRNN_grad/CudnnRNNBackpropCudnnRNNBackprop2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_11gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1+gradients_strided_slice_grad_shape_cudnnrnn%gradients_squeeze_grad_shape_cudnnrnn'gradients_squeeze_1_grad_shape_cudnnrnngradients/AddN:sum:0'gradients/Squeeze_grad/Reshape:output:0)gradients/Squeeze_1_grad/Reshape:output:0gradients_zeros_like_cudnnrnn*
T0*i
_output_shapesW
U:                  :         :         :Зќ
*gradients/transpose_grad/InvertPermutationInvertPermutation9gradients_transpose_grad_invertpermutation_transpose_perm*
_output_shapes
:┘
"gradients/transpose_grad/transpose	Transpose9gradients/CudnnRNN_grad/CudnnRNNBackprop:input_backprop:0.gradients/transpose_grad/InvertPermutation:y:0*
T0*4
_output_shapes"
 :                  u
gradients/ExpandDims_grad/ShapeShape&gradients_expanddims_grad_shape_init_h*
T0*
_output_shapes
:┼
!gradients/ExpandDims_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_h_backprop:0(gradients/ExpandDims_grad/Shape:output:0*
T0*'
_output_shapes
:         y
!gradients/ExpandDims_1_grad/ShapeShape(gradients_expanddims_1_grad_shape_init_c*
T0*
_output_shapes
:╔
#gradients/ExpandDims_1_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_c_backprop:0*gradients/ExpandDims_1_grad/Shape:output:0*
T0*'
_output_shapes
:         ^
gradients/concat_1_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :џ
gradients/concat_1_grad/modFloorMod)gradients_concat_1_grad_mod_concat_1_axis%gradients/concat_1_grad/Rank:output:0*
T0*
_output_shapes
: g
gradients/concat_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_2Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_3Const*
_output_shapes
:*
dtype0*
valueB:j
gradients/concat_1_grad/Shape_4Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_5Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_6Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_7Const*
_output_shapes
:*
dtype0*
valueB:рi
gradients/concat_1_grad/Shape_8Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_9Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_10Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_11Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_12Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_13Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_14Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_15Const*
_output_shapes
:*
dtype0*
valueB:Э
$gradients/concat_1_grad/ConcatOffsetConcatOffsetgradients/concat_1_grad/mod:z:0&gradients/concat_1_grad/Shape:output:0(gradients/concat_1_grad/Shape_1:output:0(gradients/concat_1_grad/Shape_2:output:0(gradients/concat_1_grad/Shape_3:output:0(gradients/concat_1_grad/Shape_4:output:0(gradients/concat_1_grad/Shape_5:output:0(gradients/concat_1_grad/Shape_6:output:0(gradients/concat_1_grad/Shape_7:output:0(gradients/concat_1_grad/Shape_8:output:0(gradients/concat_1_grad/Shape_9:output:0)gradients/concat_1_grad/Shape_10:output:0)gradients/concat_1_grad/Shape_11:output:0)gradients/concat_1_grad/Shape_12:output:0)gradients/concat_1_grad/Shape_13:output:0)gradients/concat_1_grad/Shape_14:output:0)gradients/concat_1_grad/Shape_15:output:0*
N*t
_output_shapesb
`::::::::::::::::в
gradients/concat_1_grad/SliceSlice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:0&gradients/concat_1_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_1Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:1(gradients/concat_1_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_2Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:2(gradients/concat_1_grad/Shape_2:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_3Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:3(gradients/concat_1_grad/Shape_3:output:0*
Index0*
T0*
_output_shapes
:­
gradients/concat_1_grad/Slice_4Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:4(gradients/concat_1_grad/Shape_4:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_5Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:5(gradients/concat_1_grad/Shape_5:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_6Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:6(gradients/concat_1_grad/Shape_6:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_7Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:7(gradients/concat_1_grad/Shape_7:output:0*
Index0*
T0*
_output_shapes	
:р№
gradients/concat_1_grad/Slice_8Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:8(gradients/concat_1_grad/Shape_8:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_9Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:9(gradients/concat_1_grad/Shape_9:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_10Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:10)gradients/concat_1_grad/Shape_10:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_11Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:11)gradients/concat_1_grad/Shape_11:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_12Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:12)gradients/concat_1_grad/Shape_12:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_13Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:13)gradients/concat_1_grad/Shape_13:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_14Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:14)gradients/concat_1_grad/Shape_14:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_15Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:15)gradients/concat_1_grad/Shape_15:output:0*
Index0*
T0*
_output_shapes
:m
gradients/Reshape_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      А
gradients/Reshape_grad/ReshapeReshape&gradients/concat_1_grad/Slice:output:0%gradients/Reshape_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_1_grad/ReshapeReshape(gradients/concat_1_grad/Slice_1:output:0'gradients/Reshape_1_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_2_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_2_grad/ReshapeReshape(gradients/concat_1_grad/Slice_2:output:0'gradients/Reshape_2_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_3_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_3_grad/ReshapeReshape(gradients/concat_1_grad/Slice_3:output:0'gradients/Reshape_3_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_4_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_4_grad/ReshapeReshape(gradients/concat_1_grad/Slice_4:output:0'gradients/Reshape_4_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_5_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_5_grad/ReshapeReshape(gradients/concat_1_grad/Slice_5:output:0'gradients/Reshape_5_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_6_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_6_grad/ReshapeReshape(gradients/concat_1_grad/Slice_6:output:0'gradients/Reshape_6_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_7_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_7_grad/ReshapeReshape(gradients/concat_1_grad/Slice_7:output:0'gradients/Reshape_7_grad/Shape:output:0*
T0*
_output_shapes

:h
gradients/Reshape_8_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_8_grad/ReshapeReshape(gradients/concat_1_grad/Slice_8:output:0'gradients/Reshape_8_grad/Shape:output:0*
T0*
_output_shapes
:h
gradients/Reshape_9_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_9_grad/ReshapeReshape(gradients/concat_1_grad/Slice_9:output:0'gradients/Reshape_9_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_10_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_10_grad/ReshapeReshape)gradients/concat_1_grad/Slice_10:output:0(gradients/Reshape_10_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_11_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_11_grad/ReshapeReshape)gradients/concat_1_grad/Slice_11:output:0(gradients/Reshape_11_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_12_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_12_grad/ReshapeReshape)gradients/concat_1_grad/Slice_12:output:0(gradients/Reshape_12_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_13_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_13_grad/ReshapeReshape)gradients/concat_1_grad/Slice_13:output:0(gradients/Reshape_13_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_14_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_14_grad/ReshapeReshape)gradients/concat_1_grad/Slice_14:output:0(gradients/Reshape_14_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_15_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_15_grad/ReshapeReshape)gradients/concat_1_grad/Slice_15:output:0(gradients/Reshape_15_grad/Shape:output:0*
T0*
_output_shapes
:ю
,gradients/transpose_1_grad/InvertPermutationInvertPermutation=gradients_transpose_1_grad_invertpermutation_transpose_1_perm*
_output_shapes
:х
$gradients/transpose_1_grad/transpose	Transpose'gradients/Reshape_grad/Reshape:output:00gradients/transpose_1_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_2_grad/InvertPermutationInvertPermutation=gradients_transpose_2_grad_invertpermutation_transpose_2_perm*
_output_shapes
:и
$gradients/transpose_2_grad/transpose	Transpose)gradients/Reshape_1_grad/Reshape:output:00gradients/transpose_2_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_3_grad/InvertPermutationInvertPermutation=gradients_transpose_3_grad_invertpermutation_transpose_3_perm*
_output_shapes
:и
$gradients/transpose_3_grad/transpose	Transpose)gradients/Reshape_2_grad/Reshape:output:00gradients/transpose_3_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_4_grad/InvertPermutationInvertPermutation=gradients_transpose_4_grad_invertpermutation_transpose_4_perm*
_output_shapes
:и
$gradients/transpose_4_grad/transpose	Transpose)gradients/Reshape_3_grad/Reshape:output:00gradients/transpose_4_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_5_grad/InvertPermutationInvertPermutation=gradients_transpose_5_grad_invertpermutation_transpose_5_perm*
_output_shapes
:и
$gradients/transpose_5_grad/transpose	Transpose)gradients/Reshape_4_grad/Reshape:output:00gradients/transpose_5_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_6_grad/InvertPermutationInvertPermutation=gradients_transpose_6_grad_invertpermutation_transpose_6_perm*
_output_shapes
:и
$gradients/transpose_6_grad/transpose	Transpose)gradients/Reshape_5_grad/Reshape:output:00gradients/transpose_6_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_7_grad/InvertPermutationInvertPermutation=gradients_transpose_7_grad_invertpermutation_transpose_7_perm*
_output_shapes
:и
$gradients/transpose_7_grad/transpose	Transpose)gradients/Reshape_6_grad/Reshape:output:00gradients/transpose_7_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_8_grad/InvertPermutationInvertPermutation=gradients_transpose_8_grad_invertpermutation_transpose_8_perm*
_output_shapes
:и
$gradients/transpose_8_grad/transpose	Transpose)gradients/Reshape_7_grad/Reshape:output:00gradients/transpose_8_grad/InvertPermutation:y:0*
T0*
_output_shapes

:Т
gradients/split_2_grad/concatConcatV2)gradients/Reshape_8_grad/Reshape:output:0)gradients/Reshape_9_grad/Reshape:output:0*gradients/Reshape_10_grad/Reshape:output:0*gradients/Reshape_11_grad/Reshape:output:0*gradients/Reshape_12_grad/Reshape:output:0*gradients/Reshape_13_grad/Reshape:output:0*gradients/Reshape_14_grad/Reshape:output:0*gradients/Reshape_15_grad/Reshape:output:0/gradients_split_2_grad_concat_split_2_split_dim*
N*
T0*
_output_shapes
:x«
gradients/split_grad/concatConcatV2(gradients/transpose_1_grad/transpose:y:0(gradients/transpose_2_grad/transpose:y:0(gradients/transpose_3_grad/transpose:y:0(gradients/transpose_4_grad/transpose:y:0+gradients_split_grad_concat_split_split_dim*
N*
T0*
_output_shapes

:<┤
gradients/split_1_grad/concatConcatV2(gradients/transpose_5_grad/transpose:y:0(gradients/transpose_6_grad/transpose:y:0(gradients/transpose_7_grad/transpose:y:0(gradients/transpose_8_grad/transpose:y:0/gradients_split_1_grad_concat_split_1_split_dim*
N*
T0*
_output_shapes

:<\
gradients/concat_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :њ
gradients/concat_grad/modFloorMod%gradients_concat_grad_mod_concat_axis#gradients/concat_grad/Rank:output:0*
T0*
_output_shapes
: e
gradients/concat_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:<g
gradients/concat_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:<╩
"gradients/concat_grad/ConcatOffsetConcatOffsetgradients/concat_grad/mod:z:0$gradients/concat_grad/Shape:output:0&gradients/concat_grad/Shape_1:output:0*
N* 
_output_shapes
::Л
gradients/concat_grad/SliceSlice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:0$gradients/concat_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:<Н
gradients/concat_grad/Slice_1Slice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:1&gradients/concat_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:<{
IdentityIdentity&gradients/transpose_grad/transpose:y:0*
T0*4
_output_shapes"
 :                  t

Identity_1Identity*gradients/ExpandDims_grad/Reshape:output:0*
T0*'
_output_shapes
:         v

Identity_2Identity,gradients/ExpandDims_1_grad/Reshape:output:0*
T0*'
_output_shapes
:         e

Identity_3Identity$gradients/split_grad/concat:output:0*
T0*
_output_shapes

:<g

Identity_4Identity&gradients/split_1_grad/concat:output:0*
T0*
_output_shapes

:<c

Identity_5Identity&gradients/concat_grad/Slice_1:output:0*
T0*
_output_shapes
:<"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"!

identity_5Identity_5:output:0*(
_construction_contextkEagerRuntime*Ѓ
_input_shapesы
Ь:         :                  :         :         : :                  ::         :         ::                  :         :         :З::         :         : ::::::::: : : : *=
api_implements+)lstm_2ce075de-0b33-47d3-b5ef-91fa50ccaa52*
api_preferred_deviceGPU*A
forward_function_name(&__forward_gpu_lstm_with_fallback_75310*
go_backwards( *

time_major( :- )
'
_output_shapes
:         ::6
4
_output_shapes"
 :                  :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: ::6
4
_output_shapes"
 :                  : 

_output_shapes
::1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :	

_output_shapes
:::
6
4
_output_shapes"
 :                  :1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :!

_output_shapes	
:З: 

_output_shapes
::-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: : 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
::

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
ї	
╝
while_cond_78817
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_78817___redundant_placeholder03
/while_while_cond_78817___redundant_placeholder13
/while_while_cond_78817___redundant_placeholder23
/while_while_cond_78817___redundant_placeholder3
while_identity
`

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: K
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: ")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*S
_input_shapesB
@: : : : :         :         : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
::

_output_shapes
::	

_output_shapes
::


_output_shapes
:
к(
╬
while_body_74481
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0
while_matmul_kernel_0%
!while_matmul_1_recurrent_kernel_0
while_biasadd_bias_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor
while_matmul_kernel#
while_matmul_1_recurrent_kernel
while_biasadd_biasѕ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       д
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:         *
element_dtype0Љ
while/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0while_matmul_kernel_0*
T0*'
_output_shapes
:         <ѓ
while/MatMul_1MatMulwhile_placeholder_2!while_matmul_1_recurrent_kernel_0*
T0*'
_output_shapes
:         <v
	while/addAddV2while/MatMul:product:0while/MatMul_1:product:0*
T0*'
_output_shapes
:         <o
while/BiasAddBiasAddwhile/add:z:0while_biasadd_bias_0*
T0*'
_output_shapes
:         <W
while/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :╚
while/splitSplitwhile/split/split_dim:output:0while/BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_split`
while/SigmoidSigmoidwhile/split:output:0*
T0*'
_output_shapes
:         b
while/Sigmoid_1Sigmoidwhile/split:output:1*
T0*'
_output_shapes
:         l
	while/mulMulwhile/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:         Z

while/TanhTanhwhile/split:output:2*
T0*'
_output_shapes
:         g
while/mul_1Mulwhile/Sigmoid:y:0while/Tanh:y:0*
T0*'
_output_shapes
:         f
while/add_1AddV2while/mul:z:0while/mul_1:z:0*
T0*'
_output_shapes
:         b
while/Sigmoid_2Sigmoidwhile/split:output:3*
T0*'
_output_shapes
:         W
while/Tanh_1Tanhwhile/add_1:z:0*
T0*'
_output_shapes
:         k
while/mul_2Mulwhile/Sigmoid_2:y:0while/Tanh_1:y:0*
T0*'
_output_shapes
:         И
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/mul_2:z:0*
_output_shapes
: *
element_dtype0:жУмO
while/add_2/yConst*
_output_shapes
: *
dtype0*
value	B :`
while/add_2AddV2while_placeholderwhile/add_2/y:output:0*
T0*
_output_shapes
: O
while/add_3/yConst*
_output_shapes
: *
dtype0*
value	B :g
while/add_3AddV2while_while_loop_counterwhile/add_3/y:output:0*
T0*
_output_shapes
: L
while/IdentityIdentitywhile/add_3:z:0*
T0*
_output_shapes
: ]
while/Identity_1Identitywhile_while_maximum_iterations*
T0*
_output_shapes
: N
while/Identity_2Identitywhile/add_2:z:0*
T0*
_output_shapes
: ї
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: :жУм_
while/Identity_4Identitywhile/mul_2:z:0*
T0*'
_output_shapes
:         _
while/Identity_5Identitywhile/add_1:z:0*
T0*'
_output_shapes
:         "*
while_biasadd_biaswhile_biasadd_bias_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"D
while_matmul_1_recurrent_kernel!while_matmul_1_recurrent_kernel_0",
while_matmul_kernelwhile_matmul_kernel_0",
while_strided_slicewhile_strided_slice_0"е
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*_
_input_shapesN
L: : : : :         :         : : :<:<:<: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:<:$	 

_output_shapes

:<: 


_output_shapes
:<
№0
щ
 __inference__wrapped_model_74404
lstm_3_inputB
0sequential_3_lstm_3_read_readvariableop_resource:<D
2sequential_3_lstm_3_read_1_readvariableop_resource:<@
2sequential_3_lstm_3_read_2_readvariableop_resource:<E
3sequential_3_dense_3_matmul_readvariableop_resource:B
4sequential_3_dense_3_biasadd_readvariableop_resource:
identityѕб+sequential_3/dense_3/BiasAdd/ReadVariableOpб*sequential_3/dense_3/MatMul/ReadVariableOpб'sequential_3/lstm_3/Read/ReadVariableOpб)sequential_3/lstm_3/Read_1/ReadVariableOpб)sequential_3/lstm_3/Read_2/ReadVariableOpU
sequential_3/lstm_3/ShapeShapelstm_3_input*
T0*
_output_shapes
:q
'sequential_3/lstm_3/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: s
)sequential_3/lstm_3/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:s
)sequential_3/lstm_3/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:х
!sequential_3/lstm_3/strided_sliceStridedSlice"sequential_3/lstm_3/Shape:output:00sequential_3/lstm_3/strided_slice/stack:output:02sequential_3/lstm_3/strided_slice/stack_1:output:02sequential_3/lstm_3/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskd
"sequential_3/lstm_3/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :»
 sequential_3/lstm_3/zeros/packedPack*sequential_3/lstm_3/strided_slice:output:0+sequential_3/lstm_3/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:d
sequential_3/lstm_3/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    е
sequential_3/lstm_3/zerosFill)sequential_3/lstm_3/zeros/packed:output:0(sequential_3/lstm_3/zeros/Const:output:0*
T0*'
_output_shapes
:         f
$sequential_3/lstm_3/zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :│
"sequential_3/lstm_3/zeros_1/packedPack*sequential_3/lstm_3/strided_slice:output:0-sequential_3/lstm_3/zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:f
!sequential_3/lstm_3/zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    «
sequential_3/lstm_3/zeros_1Fill+sequential_3/lstm_3/zeros_1/packed:output:0*sequential_3/lstm_3/zeros_1/Const:output:0*
T0*'
_output_shapes
:         _
#sequential_3/lstm_3/ones_like/ShapeShapelstm_3_input*
T0*
_output_shapes
:h
#sequential_3/lstm_3/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ђ?И
sequential_3/lstm_3/ones_likeFill,sequential_3/lstm_3/ones_like/Shape:output:0,sequential_3/lstm_3/ones_like/Const:output:0*
T0*,
_output_shapes
:         ЯІ
sequential_3/lstm_3/mulMullstm_3_input&sequential_3/lstm_3/ones_like:output:0*
T0*,
_output_shapes
:         Яў
'sequential_3/lstm_3/Read/ReadVariableOpReadVariableOp0sequential_3_lstm_3_read_readvariableop_resource*
_output_shapes

:<*
dtype0ѓ
sequential_3/lstm_3/IdentityIdentity/sequential_3/lstm_3/Read/ReadVariableOp:value:0*
T0*
_output_shapes

:<ю
)sequential_3/lstm_3/Read_1/ReadVariableOpReadVariableOp2sequential_3_lstm_3_read_1_readvariableop_resource*
_output_shapes

:<*
dtype0є
sequential_3/lstm_3/Identity_1Identity1sequential_3/lstm_3/Read_1/ReadVariableOp:value:0*
T0*
_output_shapes

:<ў
)sequential_3/lstm_3/Read_2/ReadVariableOpReadVariableOp2sequential_3_lstm_3_read_2_readvariableop_resource*
_output_shapes
:<*
dtype0ѓ
sequential_3/lstm_3/Identity_2Identity1sequential_3/lstm_3/Read_2/ReadVariableOp:value:0*
T0*
_output_shapes
:<┬
#sequential_3/lstm_3/PartitionedCallPartitionedCallsequential_3/lstm_3/mul:z:0"sequential_3/lstm_3/zeros:output:0$sequential_3/lstm_3/zeros_1:output:0%sequential_3/lstm_3/Identity:output:0'sequential_3/lstm_3/Identity_1:output:0'sequential_3/lstm_3/Identity_2:output:0*
Tin

2*
Tout	
2*
_collective_manager_ids
 *g
_output_shapesU
S:         :         Я:         :         : * 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8ѓ *(
f#R!
__inference_standard_lstm_74125ъ
*sequential_3/dense_3/MatMul/ReadVariableOpReadVariableOp3sequential_3_dense_3_matmul_readvariableop_resource*
_output_shapes

:*
dtype0╣
sequential_3/dense_3/MatMulMatMul,sequential_3/lstm_3/PartitionedCall:output:02sequential_3/dense_3/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:         ю
+sequential_3/dense_3/BiasAdd/ReadVariableOpReadVariableOp4sequential_3_dense_3_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0х
sequential_3/dense_3/BiasAddBiasAdd%sequential_3/dense_3/MatMul:product:03sequential_3/dense_3/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:         t
IdentityIdentity%sequential_3/dense_3/BiasAdd:output:0^NoOp*
T0*'
_output_shapes
:         Б
NoOpNoOp,^sequential_3/dense_3/BiasAdd/ReadVariableOp+^sequential_3/dense_3/MatMul/ReadVariableOp(^sequential_3/lstm_3/Read/ReadVariableOp*^sequential_3/lstm_3/Read_1/ReadVariableOp*^sequential_3/lstm_3/Read_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*5
_input_shapes$
":         Я: : : : : 2Z
+sequential_3/dense_3/BiasAdd/ReadVariableOp+sequential_3/dense_3/BiasAdd/ReadVariableOp2X
*sequential_3/dense_3/MatMul/ReadVariableOp*sequential_3/dense_3/MatMul/ReadVariableOp2R
'sequential_3/lstm_3/Read/ReadVariableOp'sequential_3/lstm_3/Read/ReadVariableOp2V
)sequential_3/lstm_3/Read_1/ReadVariableOp)sequential_3/lstm_3/Read_1/ReadVariableOp2V
)sequential_3/lstm_3/Read_2/ReadVariableOp)sequential_3/lstm_3/Read_2/ReadVariableOp:Z V
,
_output_shapes
:         Я
&
_user_specified_namelstm_3_input
Ж4
Њ

__inference__traced_save_79284
file_prefix-
)savev2_dense_3_kernel_read_readvariableop+
'savev2_dense_3_bias_read_readvariableop(
$savev2_adam_iter_read_readvariableop	*
&savev2_adam_beta_1_read_readvariableop*
&savev2_adam_beta_2_read_readvariableop)
%savev2_adam_decay_read_readvariableop1
-savev2_adam_learning_rate_read_readvariableop8
4savev2_lstm_3_lstm_cell_3_kernel_read_readvariableopB
>savev2_lstm_3_lstm_cell_3_recurrent_kernel_read_readvariableop6
2savev2_lstm_3_lstm_cell_3_bias_read_readvariableop$
 savev2_total_read_readvariableop$
 savev2_count_read_readvariableop4
0savev2_adam_dense_3_kernel_m_read_readvariableop2
.savev2_adam_dense_3_bias_m_read_readvariableop?
;savev2_adam_lstm_3_lstm_cell_3_kernel_m_read_readvariableopI
Esavev2_adam_lstm_3_lstm_cell_3_recurrent_kernel_m_read_readvariableop=
9savev2_adam_lstm_3_lstm_cell_3_bias_m_read_readvariableop4
0savev2_adam_dense_3_kernel_v_read_readvariableop2
.savev2_adam_dense_3_bias_v_read_readvariableop?
;savev2_adam_lstm_3_lstm_cell_3_kernel_v_read_readvariableopI
Esavev2_adam_lstm_3_lstm_cell_3_recurrent_kernel_v_read_readvariableop=
9savev2_adam_lstm_3_lstm_cell_3_bias_v_read_readvariableop
savev2_const

identity_1ѕбMergeV2Checkpointsw
StaticRegexFullMatchStaticRegexFullMatchfile_prefix"/device:CPU:**
_output_shapes
: *
pattern
^s3://.*Z
ConstConst"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B.parta
Const_1Const"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B
_temp/partЂ
SelectSelectStaticRegexFullMatch:output:0Const:output:0Const_1:output:0"/device:CPU:**
T0*
_output_shapes
: f

StringJoin
StringJoinfile_prefixSelect:output:0"/device:CPU:**
N*
_output_shapes
: L

num_shardsConst*
_output_shapes
: *
dtype0*
value	B :f
ShardedFilename/shardConst"/device:CPU:0*
_output_shapes
: *
dtype0*
value	B : Њ
ShardedFilenameShardedFilenameStringJoin:output:0ShardedFilename/shard:output:0num_shards:output:0"/device:CPU:0*
_output_shapes
: Ц
SaveV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*╬

value─
B┴
B6layer_with_weights-1/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-1/bias/.ATTRIBUTES/VARIABLE_VALUEB)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB&variables/0/.ATTRIBUTES/VARIABLE_VALUEB&variables/1/.ATTRIBUTES/VARIABLE_VALUEB&variables/2/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/0/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/1/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/2/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/0/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/1/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/2/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPHЏ
SaveV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*A
value8B6B B B B B B B B B B B B B B B B B B B B B B B ќ

SaveV2SaveV2ShardedFilename:filename:0SaveV2/tensor_names:output:0 SaveV2/shape_and_slices:output:0)savev2_dense_3_kernel_read_readvariableop'savev2_dense_3_bias_read_readvariableop$savev2_adam_iter_read_readvariableop&savev2_adam_beta_1_read_readvariableop&savev2_adam_beta_2_read_readvariableop%savev2_adam_decay_read_readvariableop-savev2_adam_learning_rate_read_readvariableop4savev2_lstm_3_lstm_cell_3_kernel_read_readvariableop>savev2_lstm_3_lstm_cell_3_recurrent_kernel_read_readvariableop2savev2_lstm_3_lstm_cell_3_bias_read_readvariableop savev2_total_read_readvariableop savev2_count_read_readvariableop0savev2_adam_dense_3_kernel_m_read_readvariableop.savev2_adam_dense_3_bias_m_read_readvariableop;savev2_adam_lstm_3_lstm_cell_3_kernel_m_read_readvariableopEsavev2_adam_lstm_3_lstm_cell_3_recurrent_kernel_m_read_readvariableop9savev2_adam_lstm_3_lstm_cell_3_bias_m_read_readvariableop0savev2_adam_dense_3_kernel_v_read_readvariableop.savev2_adam_dense_3_bias_v_read_readvariableop;savev2_adam_lstm_3_lstm_cell_3_kernel_v_read_readvariableopEsavev2_adam_lstm_3_lstm_cell_3_recurrent_kernel_v_read_readvariableop9savev2_adam_lstm_3_lstm_cell_3_bias_v_read_readvariableopsavev2_const"/device:CPU:0*
_output_shapes
 *%
dtypes
2	љ
&MergeV2Checkpoints/checkpoint_prefixesPackShardedFilename:filename:0^SaveV2"/device:CPU:0*
N*
T0*
_output_shapes
:І
MergeV2CheckpointsMergeV2Checkpoints/MergeV2Checkpoints/checkpoint_prefixes:output:0file_prefix"/device:CPU:0*
_output_shapes
 f
IdentityIdentityfile_prefix^MergeV2Checkpoints"/device:CPU:0*
T0*
_output_shapes
: Q

Identity_1IdentityIdentity:output:0^NoOp*
T0*
_output_shapes
: [
NoOpNoOp^MergeV2Checkpoints*"
_acd_function_control_output(*
_output_shapes
 "!

identity_1Identity_1:output:0*Ц
_input_shapesЊ
љ: ::: : : : : :<:<:<: : :::<:<:<:::<:<:<: 2(
MergeV2CheckpointsMergeV2Checkpoints:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix:$ 

_output_shapes

:: 

_output_shapes
::

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:<:$	 

_output_shapes

:<: 


_output_shapes
:<:

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:: 

_output_shapes
::$ 

_output_shapes

:<:$ 

_output_shapes

:<: 

_output_shapes
:<:$ 

_output_shapes

:: 

_output_shapes
::$ 

_output_shapes

:<:$ 

_output_shapes

:<: 

_output_shapes
:<:

_output_shapes
: 
к(
╬
while_body_77924
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0
while_matmul_kernel_0%
!while_matmul_1_recurrent_kernel_0
while_biasadd_bias_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor
while_matmul_kernel#
while_matmul_1_recurrent_kernel
while_biasadd_biasѕ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       д
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:         *
element_dtype0Љ
while/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0while_matmul_kernel_0*
T0*'
_output_shapes
:         <ѓ
while/MatMul_1MatMulwhile_placeholder_2!while_matmul_1_recurrent_kernel_0*
T0*'
_output_shapes
:         <v
	while/addAddV2while/MatMul:product:0while/MatMul_1:product:0*
T0*'
_output_shapes
:         <o
while/BiasAddBiasAddwhile/add:z:0while_biasadd_bias_0*
T0*'
_output_shapes
:         <W
while/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :╚
while/splitSplitwhile/split/split_dim:output:0while/BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_split`
while/SigmoidSigmoidwhile/split:output:0*
T0*'
_output_shapes
:         b
while/Sigmoid_1Sigmoidwhile/split:output:1*
T0*'
_output_shapes
:         l
	while/mulMulwhile/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:         Z

while/TanhTanhwhile/split:output:2*
T0*'
_output_shapes
:         g
while/mul_1Mulwhile/Sigmoid:y:0while/Tanh:y:0*
T0*'
_output_shapes
:         f
while/add_1AddV2while/mul:z:0while/mul_1:z:0*
T0*'
_output_shapes
:         b
while/Sigmoid_2Sigmoidwhile/split:output:3*
T0*'
_output_shapes
:         W
while/Tanh_1Tanhwhile/add_1:z:0*
T0*'
_output_shapes
:         k
while/mul_2Mulwhile/Sigmoid_2:y:0while/Tanh_1:y:0*
T0*'
_output_shapes
:         И
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/mul_2:z:0*
_output_shapes
: *
element_dtype0:жУмO
while/add_2/yConst*
_output_shapes
: *
dtype0*
value	B :`
while/add_2AddV2while_placeholderwhile/add_2/y:output:0*
T0*
_output_shapes
: O
while/add_3/yConst*
_output_shapes
: *
dtype0*
value	B :g
while/add_3AddV2while_while_loop_counterwhile/add_3/y:output:0*
T0*
_output_shapes
: L
while/IdentityIdentitywhile/add_3:z:0*
T0*
_output_shapes
: ]
while/Identity_1Identitywhile_while_maximum_iterations*
T0*
_output_shapes
: N
while/Identity_2Identitywhile/add_2:z:0*
T0*
_output_shapes
: ї
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: :жУм_
while/Identity_4Identitywhile/mul_2:z:0*
T0*'
_output_shapes
:         _
while/Identity_5Identitywhile/add_1:z:0*
T0*'
_output_shapes
:         "*
while_biasadd_biaswhile_biasadd_bias_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"D
while_matmul_1_recurrent_kernel!while_matmul_1_recurrent_kernel_0",
while_matmul_kernelwhile_matmul_kernel_0",
while_strided_slicewhile_strided_slice_0"е
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*_
_input_shapesN
L: : : : :         :         : : :<:<:<: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:<:$	 

_output_shapes

:<: 


_output_shapes
:<
у:
И
A__inference_lstm_3_layer_call_and_return_conditional_losses_76283

inputs.
read_readvariableop_resource:<0
read_1_readvariableop_resource:<,
read_2_readvariableop_resource:<

identity_3ѕбRead/ReadVariableOpбRead_1/ReadVariableOpбRead_2/ReadVariableOp;
ShapeShapeinputs*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Л
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskP
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :s
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:P
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    l
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:         R
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :w
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:R
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    r
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:         E
ones_like/ShapeShapeinputs*
T0*
_output_shapes
:T
ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ђ?|
	ones_likeFillones_like/Shape:output:0ones_like/Const:output:0*
T0*,
_output_shapes
:         ЯR
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?u
dropout/MulMulones_like:output:0dropout/Const:output:0*
T0*,
_output_shapes
:         ЯO
dropout/ShapeShapeones_like:output:0*
T0*
_output_shapes
:Љ
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*,
_output_shapes
:         Я*
dtype0[
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<Ф
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*,
_output_shapes
:         Яt
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*,
_output_shapes
:         Яn
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*,
_output_shapes
:         ЯT
dropout_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?y
dropout_1/MulMulones_like:output:0dropout_1/Const:output:0*
T0*,
_output_shapes
:         ЯQ
dropout_1/ShapeShapeones_like:output:0*
T0*
_output_shapes
:Ћ
&dropout_1/random_uniform/RandomUniformRandomUniformdropout_1/Shape:output:0*
T0*,
_output_shapes
:         Я*
dtype0]
dropout_1/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<▒
dropout_1/GreaterEqualGreaterEqual/dropout_1/random_uniform/RandomUniform:output:0!dropout_1/GreaterEqual/y:output:0*
T0*,
_output_shapes
:         Яx
dropout_1/CastCastdropout_1/GreaterEqual:z:0*

DstT0*

SrcT0
*,
_output_shapes
:         Яt
dropout_1/Mul_1Muldropout_1/Mul:z:0dropout_1/Cast:y:0*
T0*,
_output_shapes
:         ЯT
dropout_2/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?y
dropout_2/MulMulones_like:output:0dropout_2/Const:output:0*
T0*,
_output_shapes
:         ЯQ
dropout_2/ShapeShapeones_like:output:0*
T0*
_output_shapes
:Ћ
&dropout_2/random_uniform/RandomUniformRandomUniformdropout_2/Shape:output:0*
T0*,
_output_shapes
:         Я*
dtype0]
dropout_2/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<▒
dropout_2/GreaterEqualGreaterEqual/dropout_2/random_uniform/RandomUniform:output:0!dropout_2/GreaterEqual/y:output:0*
T0*,
_output_shapes
:         Яx
dropout_2/CastCastdropout_2/GreaterEqual:z:0*

DstT0*

SrcT0
*,
_output_shapes
:         Яt
dropout_2/Mul_1Muldropout_2/Mul:z:0dropout_2/Cast:y:0*
T0*,
_output_shapes
:         ЯT
dropout_3/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?y
dropout_3/MulMulones_like:output:0dropout_3/Const:output:0*
T0*,
_output_shapes
:         ЯQ
dropout_3/ShapeShapeones_like:output:0*
T0*
_output_shapes
:Ћ
&dropout_3/random_uniform/RandomUniformRandomUniformdropout_3/Shape:output:0*
T0*,
_output_shapes
:         Я*
dtype0]
dropout_3/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<▒
dropout_3/GreaterEqualGreaterEqual/dropout_3/random_uniform/RandomUniform:output:0!dropout_3/GreaterEqual/y:output:0*
T0*,
_output_shapes
:         Яx
dropout_3/CastCastdropout_3/GreaterEqual:z:0*

DstT0*

SrcT0
*,
_output_shapes
:         Яt
dropout_3/Mul_1Muldropout_3/Mul:z:0dropout_3/Cast:y:0*
T0*,
_output_shapes
:         Я\
mulMulinputsdropout/Mul_1:z:0*
T0*,
_output_shapes
:         Яp
Read/ReadVariableOpReadVariableOpread_readvariableop_resource*
_output_shapes

:<*
dtype0Z
IdentityIdentityRead/ReadVariableOp:value:0*
T0*
_output_shapes

:<t
Read_1/ReadVariableOpReadVariableOpread_1_readvariableop_resource*
_output_shapes

:<*
dtype0^

Identity_1IdentityRead_1/ReadVariableOp:value:0*
T0*
_output_shapes

:<p
Read_2/ReadVariableOpReadVariableOpread_2_readvariableop_resource*
_output_shapes
:<*
dtype0Z

Identity_2IdentityRead_2/ReadVariableOp:value:0*
T0*
_output_shapes
:<Х
PartitionedCallPartitionedCallmul:z:0zeros:output:0zeros_1:output:0Identity:output:0Identity_1:output:0Identity_2:output:0*
Tin

2*
Tout	
2*
_collective_manager_ids
 *g
_output_shapesU
S:         :         Я:         :         : * 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8ѓ *(
f#R!
__inference_standard_lstm_76010i

Identity_3IdentityPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         ї
NoOpNoOp^Read/ReadVariableOp^Read_1/ReadVariableOp^Read_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*1
_input_shapes 
:         Я: : : 2*
Read/ReadVariableOpRead/ReadVariableOp2.
Read_1/ReadVariableOpRead_1/ReadVariableOp2.
Read_2/ReadVariableOpRead_2/ReadVariableOp:T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs
к(
╬
while_body_77461
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0
while_matmul_kernel_0%
!while_matmul_1_recurrent_kernel_0
while_biasadd_bias_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor
while_matmul_kernel#
while_matmul_1_recurrent_kernel
while_biasadd_biasѕ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       д
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:         *
element_dtype0Љ
while/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0while_matmul_kernel_0*
T0*'
_output_shapes
:         <ѓ
while/MatMul_1MatMulwhile_placeholder_2!while_matmul_1_recurrent_kernel_0*
T0*'
_output_shapes
:         <v
	while/addAddV2while/MatMul:product:0while/MatMul_1:product:0*
T0*'
_output_shapes
:         <o
while/BiasAddBiasAddwhile/add:z:0while_biasadd_bias_0*
T0*'
_output_shapes
:         <W
while/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :╚
while/splitSplitwhile/split/split_dim:output:0while/BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_split`
while/SigmoidSigmoidwhile/split:output:0*
T0*'
_output_shapes
:         b
while/Sigmoid_1Sigmoidwhile/split:output:1*
T0*'
_output_shapes
:         l
	while/mulMulwhile/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:         Z

while/TanhTanhwhile/split:output:2*
T0*'
_output_shapes
:         g
while/mul_1Mulwhile/Sigmoid:y:0while/Tanh:y:0*
T0*'
_output_shapes
:         f
while/add_1AddV2while/mul:z:0while/mul_1:z:0*
T0*'
_output_shapes
:         b
while/Sigmoid_2Sigmoidwhile/split:output:3*
T0*'
_output_shapes
:         W
while/Tanh_1Tanhwhile/add_1:z:0*
T0*'
_output_shapes
:         k
while/mul_2Mulwhile/Sigmoid_2:y:0while/Tanh_1:y:0*
T0*'
_output_shapes
:         И
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/mul_2:z:0*
_output_shapes
: *
element_dtype0:жУмO
while/add_2/yConst*
_output_shapes
: *
dtype0*
value	B :`
while/add_2AddV2while_placeholderwhile/add_2/y:output:0*
T0*
_output_shapes
: O
while/add_3/yConst*
_output_shapes
: *
dtype0*
value	B :g
while/add_3AddV2while_while_loop_counterwhile/add_3/y:output:0*
T0*
_output_shapes
: L
while/IdentityIdentitywhile/add_3:z:0*
T0*
_output_shapes
: ]
while/Identity_1Identitywhile_while_maximum_iterations*
T0*
_output_shapes
: N
while/Identity_2Identitywhile/add_2:z:0*
T0*
_output_shapes
: ї
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: :жУм_
while/Identity_4Identitywhile/mul_2:z:0*
T0*'
_output_shapes
:         _
while/Identity_5Identitywhile/add_1:z:0*
T0*'
_output_shapes
:         "*
while_biasadd_biaswhile_biasadd_bias_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"D
while_matmul_1_recurrent_kernel!while_matmul_1_recurrent_kernel_0",
while_matmul_kernelwhile_matmul_kernel_0",
while_strided_slicewhile_strided_slice_0"е
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*_
_input_shapesN
L: : : : :         :         : : :<:<:<: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:<:$	 

_output_shapes

:<: 


_output_shapes
:<
а@
╦
(__inference_gpu_lstm_with_fallback_74219

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4ѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          n
	transpose	Transposeinputstranspose/perm:output:0*
T0*,
_output_shapes
:Я         P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : o

ExpandDims
ExpandDimsinit_hExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : s
ExpandDims_1
ExpandDimsinit_cExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : Ъ
concat_1ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0*
_output_shapes	
:З╦
CudnnRNNCudnnRNNtranspose:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1:output:0*
T0*^
_output_shapesL
J:Я         :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          }
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*,
_output_shapes
:         Яp
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_9:y:0*
T0*,
_output_shapes
:         ЯZ

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_0067de53-6b05-422e-aca0-48144f07387a*
api_preferred_deviceGPU*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
Ж┴
ы
9__inference___backward_gpu_lstm_with_fallback_76105_76281
placeholder
placeholder_1
placeholder_2
placeholder_3
placeholder_4/
+gradients_strided_slice_grad_shape_cudnnrnnA
=gradients_transpose_9_grad_invertpermutation_transpose_9_perm)
%gradients_squeeze_grad_shape_cudnnrnn+
'gradients_squeeze_1_grad_shape_cudnnrnn!
gradients_zeros_like_cudnnrnn6
2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose7
3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims9
5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_15
1gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1=
9gradients_transpose_grad_invertpermutation_transpose_perm*
&gradients_expanddims_grad_shape_init_h,
(gradients_expanddims_1_grad_shape_init_c-
)gradients_concat_1_grad_mod_concat_1_axisA
=gradients_transpose_1_grad_invertpermutation_transpose_1_permA
=gradients_transpose_2_grad_invertpermutation_transpose_2_permA
=gradients_transpose_3_grad_invertpermutation_transpose_3_permA
=gradients_transpose_4_grad_invertpermutation_transpose_4_permA
=gradients_transpose_5_grad_invertpermutation_transpose_5_permA
=gradients_transpose_6_grad_invertpermutation_transpose_6_permA
=gradients_transpose_7_grad_invertpermutation_transpose_7_permA
=gradients_transpose_8_grad_invertpermutation_transpose_8_perm3
/gradients_split_2_grad_concat_split_2_split_dim/
+gradients_split_grad_concat_split_split_dim3
/gradients_split_1_grad_concat_split_1_split_dim)
%gradients_concat_grad_mod_concat_axis
identity

identity_1

identity_2

identity_3

identity_4

identity_5ѕ^
gradients/grad_ys_0Identityplaceholder*
T0*'
_output_shapes
:         e
gradients/grad_ys_1Identityplaceholder_1*
T0*,
_output_shapes
:         Я`
gradients/grad_ys_2Identityplaceholder_2*
T0*'
_output_shapes
:         `
gradients/grad_ys_3Identityplaceholder_3*
T0*'
_output_shapes
:         O
gradients/grad_ys_4Identityplaceholder_4*
T0*
_output_shapes
: }
"gradients/strided_slice_grad/ShapeShape+gradients_strided_slice_grad_shape_cudnnrnn*
T0*
_output_shapes
:є
3gradients/strided_slice_grad/StridedSliceGrad/beginConst*
_output_shapes
:*
dtype0*
valueB:
         {
1gradients/strided_slice_grad/StridedSliceGrad/endConst*
_output_shapes
:*
dtype0*
valueB: 
5gradients/strided_slice_grad/StridedSliceGrad/stridesConst*
_output_shapes
:*
dtype0*
valueB:б
-gradients/strided_slice_grad/StridedSliceGradStridedSliceGrad+gradients/strided_slice_grad/Shape:output:0<gradients/strided_slice_grad/StridedSliceGrad/begin:output:0:gradients/strided_slice_grad/StridedSliceGrad/end:output:0>gradients/strided_slice_grad/StridedSliceGrad/strides:output:0gradients/grad_ys_0:output:0*
Index0*
T0*,
_output_shapes
:Я         *
shrink_axis_maskю
,gradients/transpose_9_grad/InvertPermutationInvertPermutation=gradients_transpose_9_grad_invertpermutation_transpose_9_perm*
_output_shapes
:И
$gradients/transpose_9_grad/transpose	Transposegradients/grad_ys_1:output:00gradients/transpose_9_grad/InvertPermutation:y:0*
T0*,
_output_shapes
:Я         q
gradients/Squeeze_grad/ShapeShape%gradients_squeeze_grad_shape_cudnnrnn*
T0*
_output_shapes
:ц
gradients/Squeeze_grad/ReshapeReshapegradients/grad_ys_2:output:0%gradients/Squeeze_grad/Shape:output:0*
T0*+
_output_shapes
:         u
gradients/Squeeze_1_grad/ShapeShape'gradients_squeeze_1_grad_shape_cudnnrnn*
T0*
_output_shapes
:е
 gradients/Squeeze_1_grad/ReshapeReshapegradients/grad_ys_3:output:0'gradients/Squeeze_1_grad/Shape:output:0*
T0*+
_output_shapes
:         Щ
gradients/AddNAddN6gradients/strided_slice_grad/StridedSliceGrad:output:0(gradients/transpose_9_grad/transpose:y:0*
N*
T0*@
_class6
42loc:@gradients/strided_slice_grad/StridedSliceGrad*,
_output_shapes
:Я         c
gradients/zeros_like	ZerosLikegradients_zeros_like_cudnnrnn*
T0*
_output_shapes
:Ђ
(gradients/CudnnRNN_grad/CudnnRNNBackpropCudnnRNNBackprop2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_11gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1+gradients_strided_slice_grad_shape_cudnnrnn%gradients_squeeze_grad_shape_cudnnrnn'gradients_squeeze_1_grad_shape_cudnnrnngradients/AddN:sum:0'gradients/Squeeze_grad/Reshape:output:0)gradients/Squeeze_1_grad/Reshape:output:0gradients_zeros_like_cudnnrnn*
T0*a
_output_shapesO
M:Я         :         :         :Зќ
*gradients/transpose_grad/InvertPermutationInvertPermutation9gradients_transpose_grad_invertpermutation_transpose_perm*
_output_shapes
:Л
"gradients/transpose_grad/transpose	Transpose9gradients/CudnnRNN_grad/CudnnRNNBackprop:input_backprop:0.gradients/transpose_grad/InvertPermutation:y:0*
T0*,
_output_shapes
:         Яu
gradients/ExpandDims_grad/ShapeShape&gradients_expanddims_grad_shape_init_h*
T0*
_output_shapes
:┼
!gradients/ExpandDims_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_h_backprop:0(gradients/ExpandDims_grad/Shape:output:0*
T0*'
_output_shapes
:         y
!gradients/ExpandDims_1_grad/ShapeShape(gradients_expanddims_1_grad_shape_init_c*
T0*
_output_shapes
:╔
#gradients/ExpandDims_1_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_c_backprop:0*gradients/ExpandDims_1_grad/Shape:output:0*
T0*'
_output_shapes
:         ^
gradients/concat_1_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :џ
gradients/concat_1_grad/modFloorMod)gradients_concat_1_grad_mod_concat_1_axis%gradients/concat_1_grad/Rank:output:0*
T0*
_output_shapes
: g
gradients/concat_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_2Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_3Const*
_output_shapes
:*
dtype0*
valueB:j
gradients/concat_1_grad/Shape_4Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_5Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_6Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_7Const*
_output_shapes
:*
dtype0*
valueB:рi
gradients/concat_1_grad/Shape_8Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_9Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_10Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_11Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_12Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_13Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_14Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_15Const*
_output_shapes
:*
dtype0*
valueB:Э
$gradients/concat_1_grad/ConcatOffsetConcatOffsetgradients/concat_1_grad/mod:z:0&gradients/concat_1_grad/Shape:output:0(gradients/concat_1_grad/Shape_1:output:0(gradients/concat_1_grad/Shape_2:output:0(gradients/concat_1_grad/Shape_3:output:0(gradients/concat_1_grad/Shape_4:output:0(gradients/concat_1_grad/Shape_5:output:0(gradients/concat_1_grad/Shape_6:output:0(gradients/concat_1_grad/Shape_7:output:0(gradients/concat_1_grad/Shape_8:output:0(gradients/concat_1_grad/Shape_9:output:0)gradients/concat_1_grad/Shape_10:output:0)gradients/concat_1_grad/Shape_11:output:0)gradients/concat_1_grad/Shape_12:output:0)gradients/concat_1_grad/Shape_13:output:0)gradients/concat_1_grad/Shape_14:output:0)gradients/concat_1_grad/Shape_15:output:0*
N*t
_output_shapesb
`::::::::::::::::в
gradients/concat_1_grad/SliceSlice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:0&gradients/concat_1_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_1Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:1(gradients/concat_1_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_2Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:2(gradients/concat_1_grad/Shape_2:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_3Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:3(gradients/concat_1_grad/Shape_3:output:0*
Index0*
T0*
_output_shapes
:­
gradients/concat_1_grad/Slice_4Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:4(gradients/concat_1_grad/Shape_4:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_5Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:5(gradients/concat_1_grad/Shape_5:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_6Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:6(gradients/concat_1_grad/Shape_6:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_7Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:7(gradients/concat_1_grad/Shape_7:output:0*
Index0*
T0*
_output_shapes	
:р№
gradients/concat_1_grad/Slice_8Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:8(gradients/concat_1_grad/Shape_8:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_9Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:9(gradients/concat_1_grad/Shape_9:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_10Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:10)gradients/concat_1_grad/Shape_10:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_11Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:11)gradients/concat_1_grad/Shape_11:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_12Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:12)gradients/concat_1_grad/Shape_12:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_13Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:13)gradients/concat_1_grad/Shape_13:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_14Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:14)gradients/concat_1_grad/Shape_14:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_15Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:15)gradients/concat_1_grad/Shape_15:output:0*
Index0*
T0*
_output_shapes
:m
gradients/Reshape_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      А
gradients/Reshape_grad/ReshapeReshape&gradients/concat_1_grad/Slice:output:0%gradients/Reshape_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_1_grad/ReshapeReshape(gradients/concat_1_grad/Slice_1:output:0'gradients/Reshape_1_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_2_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_2_grad/ReshapeReshape(gradients/concat_1_grad/Slice_2:output:0'gradients/Reshape_2_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_3_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_3_grad/ReshapeReshape(gradients/concat_1_grad/Slice_3:output:0'gradients/Reshape_3_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_4_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_4_grad/ReshapeReshape(gradients/concat_1_grad/Slice_4:output:0'gradients/Reshape_4_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_5_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_5_grad/ReshapeReshape(gradients/concat_1_grad/Slice_5:output:0'gradients/Reshape_5_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_6_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_6_grad/ReshapeReshape(gradients/concat_1_grad/Slice_6:output:0'gradients/Reshape_6_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_7_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_7_grad/ReshapeReshape(gradients/concat_1_grad/Slice_7:output:0'gradients/Reshape_7_grad/Shape:output:0*
T0*
_output_shapes

:h
gradients/Reshape_8_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_8_grad/ReshapeReshape(gradients/concat_1_grad/Slice_8:output:0'gradients/Reshape_8_grad/Shape:output:0*
T0*
_output_shapes
:h
gradients/Reshape_9_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_9_grad/ReshapeReshape(gradients/concat_1_grad/Slice_9:output:0'gradients/Reshape_9_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_10_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_10_grad/ReshapeReshape)gradients/concat_1_grad/Slice_10:output:0(gradients/Reshape_10_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_11_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_11_grad/ReshapeReshape)gradients/concat_1_grad/Slice_11:output:0(gradients/Reshape_11_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_12_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_12_grad/ReshapeReshape)gradients/concat_1_grad/Slice_12:output:0(gradients/Reshape_12_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_13_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_13_grad/ReshapeReshape)gradients/concat_1_grad/Slice_13:output:0(gradients/Reshape_13_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_14_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_14_grad/ReshapeReshape)gradients/concat_1_grad/Slice_14:output:0(gradients/Reshape_14_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_15_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_15_grad/ReshapeReshape)gradients/concat_1_grad/Slice_15:output:0(gradients/Reshape_15_grad/Shape:output:0*
T0*
_output_shapes
:ю
,gradients/transpose_1_grad/InvertPermutationInvertPermutation=gradients_transpose_1_grad_invertpermutation_transpose_1_perm*
_output_shapes
:х
$gradients/transpose_1_grad/transpose	Transpose'gradients/Reshape_grad/Reshape:output:00gradients/transpose_1_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_2_grad/InvertPermutationInvertPermutation=gradients_transpose_2_grad_invertpermutation_transpose_2_perm*
_output_shapes
:и
$gradients/transpose_2_grad/transpose	Transpose)gradients/Reshape_1_grad/Reshape:output:00gradients/transpose_2_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_3_grad/InvertPermutationInvertPermutation=gradients_transpose_3_grad_invertpermutation_transpose_3_perm*
_output_shapes
:и
$gradients/transpose_3_grad/transpose	Transpose)gradients/Reshape_2_grad/Reshape:output:00gradients/transpose_3_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_4_grad/InvertPermutationInvertPermutation=gradients_transpose_4_grad_invertpermutation_transpose_4_perm*
_output_shapes
:и
$gradients/transpose_4_grad/transpose	Transpose)gradients/Reshape_3_grad/Reshape:output:00gradients/transpose_4_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_5_grad/InvertPermutationInvertPermutation=gradients_transpose_5_grad_invertpermutation_transpose_5_perm*
_output_shapes
:и
$gradients/transpose_5_grad/transpose	Transpose)gradients/Reshape_4_grad/Reshape:output:00gradients/transpose_5_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_6_grad/InvertPermutationInvertPermutation=gradients_transpose_6_grad_invertpermutation_transpose_6_perm*
_output_shapes
:и
$gradients/transpose_6_grad/transpose	Transpose)gradients/Reshape_5_grad/Reshape:output:00gradients/transpose_6_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_7_grad/InvertPermutationInvertPermutation=gradients_transpose_7_grad_invertpermutation_transpose_7_perm*
_output_shapes
:и
$gradients/transpose_7_grad/transpose	Transpose)gradients/Reshape_6_grad/Reshape:output:00gradients/transpose_7_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_8_grad/InvertPermutationInvertPermutation=gradients_transpose_8_grad_invertpermutation_transpose_8_perm*
_output_shapes
:и
$gradients/transpose_8_grad/transpose	Transpose)gradients/Reshape_7_grad/Reshape:output:00gradients/transpose_8_grad/InvertPermutation:y:0*
T0*
_output_shapes

:Т
gradients/split_2_grad/concatConcatV2)gradients/Reshape_8_grad/Reshape:output:0)gradients/Reshape_9_grad/Reshape:output:0*gradients/Reshape_10_grad/Reshape:output:0*gradients/Reshape_11_grad/Reshape:output:0*gradients/Reshape_12_grad/Reshape:output:0*gradients/Reshape_13_grad/Reshape:output:0*gradients/Reshape_14_grad/Reshape:output:0*gradients/Reshape_15_grad/Reshape:output:0/gradients_split_2_grad_concat_split_2_split_dim*
N*
T0*
_output_shapes
:x«
gradients/split_grad/concatConcatV2(gradients/transpose_1_grad/transpose:y:0(gradients/transpose_2_grad/transpose:y:0(gradients/transpose_3_grad/transpose:y:0(gradients/transpose_4_grad/transpose:y:0+gradients_split_grad_concat_split_split_dim*
N*
T0*
_output_shapes

:<┤
gradients/split_1_grad/concatConcatV2(gradients/transpose_5_grad/transpose:y:0(gradients/transpose_6_grad/transpose:y:0(gradients/transpose_7_grad/transpose:y:0(gradients/transpose_8_grad/transpose:y:0/gradients_split_1_grad_concat_split_1_split_dim*
N*
T0*
_output_shapes

:<\
gradients/concat_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :њ
gradients/concat_grad/modFloorMod%gradients_concat_grad_mod_concat_axis#gradients/concat_grad/Rank:output:0*
T0*
_output_shapes
: e
gradients/concat_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:<g
gradients/concat_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:<╩
"gradients/concat_grad/ConcatOffsetConcatOffsetgradients/concat_grad/mod:z:0$gradients/concat_grad/Shape:output:0&gradients/concat_grad/Shape_1:output:0*
N* 
_output_shapes
::Л
gradients/concat_grad/SliceSlice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:0$gradients/concat_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:<Н
gradients/concat_grad/Slice_1Slice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:1&gradients/concat_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:<s
IdentityIdentity&gradients/transpose_grad/transpose:y:0*
T0*,
_output_shapes
:         Яt

Identity_1Identity*gradients/ExpandDims_grad/Reshape:output:0*
T0*'
_output_shapes
:         v

Identity_2Identity,gradients/ExpandDims_1_grad/Reshape:output:0*
T0*'
_output_shapes
:         e

Identity_3Identity$gradients/split_grad/concat:output:0*
T0*
_output_shapes

:<g

Identity_4Identity&gradients/split_1_grad/concat:output:0*
T0*
_output_shapes

:<c

Identity_5Identity&gradients/concat_grad/Slice_1:output:0*
T0*
_output_shapes
:<"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"!

identity_5Identity_5:output:0*(
_construction_contextkEagerRuntime*в
_input_shapes┘
о:         :         Я:         :         : :Я         ::         :         ::Я         :         :         :З::         :         : ::::::::: : : : *=
api_implements+)lstm_017ba245-194f-42f6-9988-067e14309e69*
api_preferred_deviceGPU*A
forward_function_name(&__forward_gpu_lstm_with_fallback_76280*
go_backwards( *

time_major( :- )
'
_output_shapes
:         :2.
,
_output_shapes
:         Я:-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :2.
,
_output_shapes
:Я         : 

_output_shapes
::1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :	

_output_shapes
::2
.
,
_output_shapes
:Я         :1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :!

_output_shapes	
:З: 

_output_shapes
::-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: : 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
::

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
§K
Б
&__forward_gpu_lstm_with_fallback_78710

inputs
init_h_0
init_c_0

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4
cudnnrnn
transpose_9_perm

cudnnrnn_0

cudnnrnn_1

cudnnrnn_2
	transpose

expanddims
expanddims_1
concat_1
transpose_perm

init_h

init_c
concat_1_axis
transpose_1_perm
transpose_2_perm
transpose_3_perm
transpose_4_perm
transpose_5_perm
transpose_6_perm
transpose_7_perm
transpose_8_perm
split_2_split_dim
split_split_dim
split_1_split_dim
concat_axisѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          B
transpose_0	Transposeinputstranspose/perm:output:0*
T0P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : q

ExpandDims
ExpandDimsinit_h_0ExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : u
ExpandDims_1
ExpandDimsinit_c_0ExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : ё

concat_1_0ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0¤
CudnnRNNCudnnRNNtranspose_0:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1_0:output:0*
T0*^
_output_shapesL
J:Я         :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          }
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*,
_output_shapes
:         Яp
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_9:y:0*
T0*,
_output_shapes
:         ЯZ

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
concat_1concat_1_0:output:0"'
concat_1_axisconcat_1/axis:output:0"#
concat_axisconcat/axis:output:0"
cudnnrnnCudnnRNN:output:0"!

cudnnrnn_0CudnnRNN:output_h:0"!

cudnnrnn_1CudnnRNN:output_c:0"&

cudnnrnn_2CudnnRNN:reserve_space:0"!

expanddimsExpandDims:output:0"%
expanddims_1ExpandDims_1:output:0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"
init_cinit_c_0"
init_hinit_h_0"/
split_1_split_dimsplit_1/split_dim:output:0"/
split_2_split_dimsplit_2/split_dim:output:0"+
split_split_dimsplit/split_dim:output:0"
	transposetranspose_0:y:0"-
transpose_1_permtranspose_1/perm:output:0"-
transpose_2_permtranspose_2/perm:output:0"-
transpose_3_permtranspose_3/perm:output:0"-
transpose_4_permtranspose_4/perm:output:0"-
transpose_5_permtranspose_5/perm:output:0"-
transpose_6_permtranspose_6/perm:output:0"-
transpose_7_permtranspose_7/perm:output:0"-
transpose_8_permtranspose_8/perm:output:0"-
transpose_9_permtranspose_9/perm:output:0")
transpose_permtranspose/perm:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_c1189ae0-4eec-42a2-962a-5abd558aa8a4*
api_preferred_deviceGPU*U
backward_function_name;9__inference___backward_gpu_lstm_with_fallback_78535_78711*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
Л@
╦
(__inference_gpu_lstm_with_fallback_78103

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4ѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          v
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :                  P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : o

ExpandDims
ExpandDimsinit_hExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : s
ExpandDims_1
ExpandDimsinit_cExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : Ъ
concat_1ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0*
_output_shapes	
:ЗМ
CudnnRNNCudnnRNNtranspose:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1:output:0*
T0*f
_output_shapesT
R:                  :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          Ё
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*4
_output_shapes"
 :                  p
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         f

Identity_1Identitytranspose_9:y:0*
T0*4
_output_shapes"
 :                  Z

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*s
_input_shapesb
`:                  :         :         :<:<:<*=
api_implements+)lstm_bb744fac-7292-4ecb-affd-a726f7708b09*
api_preferred_deviceGPU*
go_backwards( *

time_major( :\ X
4
_output_shapes"
 :                  
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
Ж┴
ы
9__inference___backward_gpu_lstm_with_fallback_78998_79174
placeholder
placeholder_1
placeholder_2
placeholder_3
placeholder_4/
+gradients_strided_slice_grad_shape_cudnnrnnA
=gradients_transpose_9_grad_invertpermutation_transpose_9_perm)
%gradients_squeeze_grad_shape_cudnnrnn+
'gradients_squeeze_1_grad_shape_cudnnrnn!
gradients_zeros_like_cudnnrnn6
2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose7
3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims9
5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_15
1gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1=
9gradients_transpose_grad_invertpermutation_transpose_perm*
&gradients_expanddims_grad_shape_init_h,
(gradients_expanddims_1_grad_shape_init_c-
)gradients_concat_1_grad_mod_concat_1_axisA
=gradients_transpose_1_grad_invertpermutation_transpose_1_permA
=gradients_transpose_2_grad_invertpermutation_transpose_2_permA
=gradients_transpose_3_grad_invertpermutation_transpose_3_permA
=gradients_transpose_4_grad_invertpermutation_transpose_4_permA
=gradients_transpose_5_grad_invertpermutation_transpose_5_permA
=gradients_transpose_6_grad_invertpermutation_transpose_6_permA
=gradients_transpose_7_grad_invertpermutation_transpose_7_permA
=gradients_transpose_8_grad_invertpermutation_transpose_8_perm3
/gradients_split_2_grad_concat_split_2_split_dim/
+gradients_split_grad_concat_split_split_dim3
/gradients_split_1_grad_concat_split_1_split_dim)
%gradients_concat_grad_mod_concat_axis
identity

identity_1

identity_2

identity_3

identity_4

identity_5ѕ^
gradients/grad_ys_0Identityplaceholder*
T0*'
_output_shapes
:         e
gradients/grad_ys_1Identityplaceholder_1*
T0*,
_output_shapes
:         Я`
gradients/grad_ys_2Identityplaceholder_2*
T0*'
_output_shapes
:         `
gradients/grad_ys_3Identityplaceholder_3*
T0*'
_output_shapes
:         O
gradients/grad_ys_4Identityplaceholder_4*
T0*
_output_shapes
: }
"gradients/strided_slice_grad/ShapeShape+gradients_strided_slice_grad_shape_cudnnrnn*
T0*
_output_shapes
:є
3gradients/strided_slice_grad/StridedSliceGrad/beginConst*
_output_shapes
:*
dtype0*
valueB:
         {
1gradients/strided_slice_grad/StridedSliceGrad/endConst*
_output_shapes
:*
dtype0*
valueB: 
5gradients/strided_slice_grad/StridedSliceGrad/stridesConst*
_output_shapes
:*
dtype0*
valueB:б
-gradients/strided_slice_grad/StridedSliceGradStridedSliceGrad+gradients/strided_slice_grad/Shape:output:0<gradients/strided_slice_grad/StridedSliceGrad/begin:output:0:gradients/strided_slice_grad/StridedSliceGrad/end:output:0>gradients/strided_slice_grad/StridedSliceGrad/strides:output:0gradients/grad_ys_0:output:0*
Index0*
T0*,
_output_shapes
:Я         *
shrink_axis_maskю
,gradients/transpose_9_grad/InvertPermutationInvertPermutation=gradients_transpose_9_grad_invertpermutation_transpose_9_perm*
_output_shapes
:И
$gradients/transpose_9_grad/transpose	Transposegradients/grad_ys_1:output:00gradients/transpose_9_grad/InvertPermutation:y:0*
T0*,
_output_shapes
:Я         q
gradients/Squeeze_grad/ShapeShape%gradients_squeeze_grad_shape_cudnnrnn*
T0*
_output_shapes
:ц
gradients/Squeeze_grad/ReshapeReshapegradients/grad_ys_2:output:0%gradients/Squeeze_grad/Shape:output:0*
T0*+
_output_shapes
:         u
gradients/Squeeze_1_grad/ShapeShape'gradients_squeeze_1_grad_shape_cudnnrnn*
T0*
_output_shapes
:е
 gradients/Squeeze_1_grad/ReshapeReshapegradients/grad_ys_3:output:0'gradients/Squeeze_1_grad/Shape:output:0*
T0*+
_output_shapes
:         Щ
gradients/AddNAddN6gradients/strided_slice_grad/StridedSliceGrad:output:0(gradients/transpose_9_grad/transpose:y:0*
N*
T0*@
_class6
42loc:@gradients/strided_slice_grad/StridedSliceGrad*,
_output_shapes
:Я         c
gradients/zeros_like	ZerosLikegradients_zeros_like_cudnnrnn*
T0*
_output_shapes
:Ђ
(gradients/CudnnRNN_grad/CudnnRNNBackpropCudnnRNNBackprop2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_11gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1+gradients_strided_slice_grad_shape_cudnnrnn%gradients_squeeze_grad_shape_cudnnrnn'gradients_squeeze_1_grad_shape_cudnnrnngradients/AddN:sum:0'gradients/Squeeze_grad/Reshape:output:0)gradients/Squeeze_1_grad/Reshape:output:0gradients_zeros_like_cudnnrnn*
T0*a
_output_shapesO
M:Я         :         :         :Зќ
*gradients/transpose_grad/InvertPermutationInvertPermutation9gradients_transpose_grad_invertpermutation_transpose_perm*
_output_shapes
:Л
"gradients/transpose_grad/transpose	Transpose9gradients/CudnnRNN_grad/CudnnRNNBackprop:input_backprop:0.gradients/transpose_grad/InvertPermutation:y:0*
T0*,
_output_shapes
:         Яu
gradients/ExpandDims_grad/ShapeShape&gradients_expanddims_grad_shape_init_h*
T0*
_output_shapes
:┼
!gradients/ExpandDims_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_h_backprop:0(gradients/ExpandDims_grad/Shape:output:0*
T0*'
_output_shapes
:         y
!gradients/ExpandDims_1_grad/ShapeShape(gradients_expanddims_1_grad_shape_init_c*
T0*
_output_shapes
:╔
#gradients/ExpandDims_1_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_c_backprop:0*gradients/ExpandDims_1_grad/Shape:output:0*
T0*'
_output_shapes
:         ^
gradients/concat_1_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :џ
gradients/concat_1_grad/modFloorMod)gradients_concat_1_grad_mod_concat_1_axis%gradients/concat_1_grad/Rank:output:0*
T0*
_output_shapes
: g
gradients/concat_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_2Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_3Const*
_output_shapes
:*
dtype0*
valueB:j
gradients/concat_1_grad/Shape_4Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_5Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_6Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_7Const*
_output_shapes
:*
dtype0*
valueB:рi
gradients/concat_1_grad/Shape_8Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_9Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_10Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_11Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_12Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_13Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_14Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_15Const*
_output_shapes
:*
dtype0*
valueB:Э
$gradients/concat_1_grad/ConcatOffsetConcatOffsetgradients/concat_1_grad/mod:z:0&gradients/concat_1_grad/Shape:output:0(gradients/concat_1_grad/Shape_1:output:0(gradients/concat_1_grad/Shape_2:output:0(gradients/concat_1_grad/Shape_3:output:0(gradients/concat_1_grad/Shape_4:output:0(gradients/concat_1_grad/Shape_5:output:0(gradients/concat_1_grad/Shape_6:output:0(gradients/concat_1_grad/Shape_7:output:0(gradients/concat_1_grad/Shape_8:output:0(gradients/concat_1_grad/Shape_9:output:0)gradients/concat_1_grad/Shape_10:output:0)gradients/concat_1_grad/Shape_11:output:0)gradients/concat_1_grad/Shape_12:output:0)gradients/concat_1_grad/Shape_13:output:0)gradients/concat_1_grad/Shape_14:output:0)gradients/concat_1_grad/Shape_15:output:0*
N*t
_output_shapesb
`::::::::::::::::в
gradients/concat_1_grad/SliceSlice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:0&gradients/concat_1_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_1Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:1(gradients/concat_1_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_2Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:2(gradients/concat_1_grad/Shape_2:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_3Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:3(gradients/concat_1_grad/Shape_3:output:0*
Index0*
T0*
_output_shapes
:­
gradients/concat_1_grad/Slice_4Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:4(gradients/concat_1_grad/Shape_4:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_5Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:5(gradients/concat_1_grad/Shape_5:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_6Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:6(gradients/concat_1_grad/Shape_6:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_7Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:7(gradients/concat_1_grad/Shape_7:output:0*
Index0*
T0*
_output_shapes	
:р№
gradients/concat_1_grad/Slice_8Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:8(gradients/concat_1_grad/Shape_8:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_9Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:9(gradients/concat_1_grad/Shape_9:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_10Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:10)gradients/concat_1_grad/Shape_10:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_11Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:11)gradients/concat_1_grad/Shape_11:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_12Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:12)gradients/concat_1_grad/Shape_12:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_13Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:13)gradients/concat_1_grad/Shape_13:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_14Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:14)gradients/concat_1_grad/Shape_14:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_15Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:15)gradients/concat_1_grad/Shape_15:output:0*
Index0*
T0*
_output_shapes
:m
gradients/Reshape_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      А
gradients/Reshape_grad/ReshapeReshape&gradients/concat_1_grad/Slice:output:0%gradients/Reshape_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_1_grad/ReshapeReshape(gradients/concat_1_grad/Slice_1:output:0'gradients/Reshape_1_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_2_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_2_grad/ReshapeReshape(gradients/concat_1_grad/Slice_2:output:0'gradients/Reshape_2_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_3_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_3_grad/ReshapeReshape(gradients/concat_1_grad/Slice_3:output:0'gradients/Reshape_3_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_4_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_4_grad/ReshapeReshape(gradients/concat_1_grad/Slice_4:output:0'gradients/Reshape_4_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_5_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_5_grad/ReshapeReshape(gradients/concat_1_grad/Slice_5:output:0'gradients/Reshape_5_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_6_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_6_grad/ReshapeReshape(gradients/concat_1_grad/Slice_6:output:0'gradients/Reshape_6_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_7_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_7_grad/ReshapeReshape(gradients/concat_1_grad/Slice_7:output:0'gradients/Reshape_7_grad/Shape:output:0*
T0*
_output_shapes

:h
gradients/Reshape_8_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_8_grad/ReshapeReshape(gradients/concat_1_grad/Slice_8:output:0'gradients/Reshape_8_grad/Shape:output:0*
T0*
_output_shapes
:h
gradients/Reshape_9_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_9_grad/ReshapeReshape(gradients/concat_1_grad/Slice_9:output:0'gradients/Reshape_9_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_10_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_10_grad/ReshapeReshape)gradients/concat_1_grad/Slice_10:output:0(gradients/Reshape_10_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_11_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_11_grad/ReshapeReshape)gradients/concat_1_grad/Slice_11:output:0(gradients/Reshape_11_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_12_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_12_grad/ReshapeReshape)gradients/concat_1_grad/Slice_12:output:0(gradients/Reshape_12_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_13_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_13_grad/ReshapeReshape)gradients/concat_1_grad/Slice_13:output:0(gradients/Reshape_13_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_14_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_14_grad/ReshapeReshape)gradients/concat_1_grad/Slice_14:output:0(gradients/Reshape_14_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_15_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_15_grad/ReshapeReshape)gradients/concat_1_grad/Slice_15:output:0(gradients/Reshape_15_grad/Shape:output:0*
T0*
_output_shapes
:ю
,gradients/transpose_1_grad/InvertPermutationInvertPermutation=gradients_transpose_1_grad_invertpermutation_transpose_1_perm*
_output_shapes
:х
$gradients/transpose_1_grad/transpose	Transpose'gradients/Reshape_grad/Reshape:output:00gradients/transpose_1_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_2_grad/InvertPermutationInvertPermutation=gradients_transpose_2_grad_invertpermutation_transpose_2_perm*
_output_shapes
:и
$gradients/transpose_2_grad/transpose	Transpose)gradients/Reshape_1_grad/Reshape:output:00gradients/transpose_2_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_3_grad/InvertPermutationInvertPermutation=gradients_transpose_3_grad_invertpermutation_transpose_3_perm*
_output_shapes
:и
$gradients/transpose_3_grad/transpose	Transpose)gradients/Reshape_2_grad/Reshape:output:00gradients/transpose_3_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_4_grad/InvertPermutationInvertPermutation=gradients_transpose_4_grad_invertpermutation_transpose_4_perm*
_output_shapes
:и
$gradients/transpose_4_grad/transpose	Transpose)gradients/Reshape_3_grad/Reshape:output:00gradients/transpose_4_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_5_grad/InvertPermutationInvertPermutation=gradients_transpose_5_grad_invertpermutation_transpose_5_perm*
_output_shapes
:и
$gradients/transpose_5_grad/transpose	Transpose)gradients/Reshape_4_grad/Reshape:output:00gradients/transpose_5_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_6_grad/InvertPermutationInvertPermutation=gradients_transpose_6_grad_invertpermutation_transpose_6_perm*
_output_shapes
:и
$gradients/transpose_6_grad/transpose	Transpose)gradients/Reshape_5_grad/Reshape:output:00gradients/transpose_6_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_7_grad/InvertPermutationInvertPermutation=gradients_transpose_7_grad_invertpermutation_transpose_7_perm*
_output_shapes
:и
$gradients/transpose_7_grad/transpose	Transpose)gradients/Reshape_6_grad/Reshape:output:00gradients/transpose_7_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_8_grad/InvertPermutationInvertPermutation=gradients_transpose_8_grad_invertpermutation_transpose_8_perm*
_output_shapes
:и
$gradients/transpose_8_grad/transpose	Transpose)gradients/Reshape_7_grad/Reshape:output:00gradients/transpose_8_grad/InvertPermutation:y:0*
T0*
_output_shapes

:Т
gradients/split_2_grad/concatConcatV2)gradients/Reshape_8_grad/Reshape:output:0)gradients/Reshape_9_grad/Reshape:output:0*gradients/Reshape_10_grad/Reshape:output:0*gradients/Reshape_11_grad/Reshape:output:0*gradients/Reshape_12_grad/Reshape:output:0*gradients/Reshape_13_grad/Reshape:output:0*gradients/Reshape_14_grad/Reshape:output:0*gradients/Reshape_15_grad/Reshape:output:0/gradients_split_2_grad_concat_split_2_split_dim*
N*
T0*
_output_shapes
:x«
gradients/split_grad/concatConcatV2(gradients/transpose_1_grad/transpose:y:0(gradients/transpose_2_grad/transpose:y:0(gradients/transpose_3_grad/transpose:y:0(gradients/transpose_4_grad/transpose:y:0+gradients_split_grad_concat_split_split_dim*
N*
T0*
_output_shapes

:<┤
gradients/split_1_grad/concatConcatV2(gradients/transpose_5_grad/transpose:y:0(gradients/transpose_6_grad/transpose:y:0(gradients/transpose_7_grad/transpose:y:0(gradients/transpose_8_grad/transpose:y:0/gradients_split_1_grad_concat_split_1_split_dim*
N*
T0*
_output_shapes

:<\
gradients/concat_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :њ
gradients/concat_grad/modFloorMod%gradients_concat_grad_mod_concat_axis#gradients/concat_grad/Rank:output:0*
T0*
_output_shapes
: e
gradients/concat_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:<g
gradients/concat_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:<╩
"gradients/concat_grad/ConcatOffsetConcatOffsetgradients/concat_grad/mod:z:0$gradients/concat_grad/Shape:output:0&gradients/concat_grad/Shape_1:output:0*
N* 
_output_shapes
::Л
gradients/concat_grad/SliceSlice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:0$gradients/concat_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:<Н
gradients/concat_grad/Slice_1Slice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:1&gradients/concat_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:<s
IdentityIdentity&gradients/transpose_grad/transpose:y:0*
T0*,
_output_shapes
:         Яt

Identity_1Identity*gradients/ExpandDims_grad/Reshape:output:0*
T0*'
_output_shapes
:         v

Identity_2Identity,gradients/ExpandDims_1_grad/Reshape:output:0*
T0*'
_output_shapes
:         e

Identity_3Identity$gradients/split_grad/concat:output:0*
T0*
_output_shapes

:<g

Identity_4Identity&gradients/split_1_grad/concat:output:0*
T0*
_output_shapes

:<c

Identity_5Identity&gradients/concat_grad/Slice_1:output:0*
T0*
_output_shapes
:<"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"!

identity_5Identity_5:output:0*(
_construction_contextkEagerRuntime*в
_input_shapes┘
о:         :         Я:         :         : :Я         ::         :         ::Я         :         :         :З::         :         : ::::::::: : : : *=
api_implements+)lstm_428e9d17-ff82-4115-bdf7-a5882938e408*
api_preferred_deviceGPU*A
forward_function_name(&__forward_gpu_lstm_with_fallback_79173*
go_backwards( *

time_major( :- )
'
_output_shapes
:         :2.
,
_output_shapes
:         Я:-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :2.
,
_output_shapes
:Я         : 

_output_shapes
::1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :	

_output_shapes
::2
.
,
_output_shapes
:Я         :1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :!

_output_shapes	
:З: 

_output_shapes
::-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: : 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
::

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
к(
╬
while_body_74040
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0
while_matmul_kernel_0%
!while_matmul_1_recurrent_kernel_0
while_biasadd_bias_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor
while_matmul_kernel#
while_matmul_1_recurrent_kernel
while_biasadd_biasѕ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       д
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:         *
element_dtype0Љ
while/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0while_matmul_kernel_0*
T0*'
_output_shapes
:         <ѓ
while/MatMul_1MatMulwhile_placeholder_2!while_matmul_1_recurrent_kernel_0*
T0*'
_output_shapes
:         <v
	while/addAddV2while/MatMul:product:0while/MatMul_1:product:0*
T0*'
_output_shapes
:         <o
while/BiasAddBiasAddwhile/add:z:0while_biasadd_bias_0*
T0*'
_output_shapes
:         <W
while/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :╚
while/splitSplitwhile/split/split_dim:output:0while/BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_split`
while/SigmoidSigmoidwhile/split:output:0*
T0*'
_output_shapes
:         b
while/Sigmoid_1Sigmoidwhile/split:output:1*
T0*'
_output_shapes
:         l
	while/mulMulwhile/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:         Z

while/TanhTanhwhile/split:output:2*
T0*'
_output_shapes
:         g
while/mul_1Mulwhile/Sigmoid:y:0while/Tanh:y:0*
T0*'
_output_shapes
:         f
while/add_1AddV2while/mul:z:0while/mul_1:z:0*
T0*'
_output_shapes
:         b
while/Sigmoid_2Sigmoidwhile/split:output:3*
T0*'
_output_shapes
:         W
while/Tanh_1Tanhwhile/add_1:z:0*
T0*'
_output_shapes
:         k
while/mul_2Mulwhile/Sigmoid_2:y:0while/Tanh_1:y:0*
T0*'
_output_shapes
:         И
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/mul_2:z:0*
_output_shapes
: *
element_dtype0:жУмO
while/add_2/yConst*
_output_shapes
: *
dtype0*
value	B :`
while/add_2AddV2while_placeholderwhile/add_2/y:output:0*
T0*
_output_shapes
: O
while/add_3/yConst*
_output_shapes
: *
dtype0*
value	B :g
while/add_3AddV2while_while_loop_counterwhile/add_3/y:output:0*
T0*
_output_shapes
: L
while/IdentityIdentitywhile/add_3:z:0*
T0*
_output_shapes
: ]
while/Identity_1Identitywhile_while_maximum_iterations*
T0*
_output_shapes
: N
while/Identity_2Identitywhile/add_2:z:0*
T0*
_output_shapes
: ї
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: :жУм_
while/Identity_4Identitywhile/mul_2:z:0*
T0*'
_output_shapes
:         _
while/Identity_5Identitywhile/add_1:z:0*
T0*'
_output_shapes
:         "*
while_biasadd_biaswhile_biasadd_bias_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"D
while_matmul_1_recurrent_kernel!while_matmul_1_recurrent_kernel_0",
while_matmul_kernelwhile_matmul_kernel_0",
while_strided_slicewhile_strided_slice_0"е
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*_
_input_shapesN
L: : : : :         :         : : :<:<:<: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:<:$	 

_output_shapes

:<: 


_output_shapes
:<
ш
░
&__inference_lstm_3_layer_call_fn_77388

inputs
unknown:<
	unknown_0:<
	unknown_1:<
identityѕбStatefulPartitionedCallТ
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:         *%
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8ѓ *J
fERC
A__inference_lstm_3_layer_call_and_return_conditional_losses_76283o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         `
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*1
_input_shapes 
:         Я: : : 22
StatefulPartitionedCallStatefulPartitionedCall:T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs
Њ:
┐
__inference_standard_lstm_74125

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4c
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          n
	transpose	Transposeinputstranspose/perm:output:0*
T0*,
_output_shapes
:Я         B
ShapeShapetranspose:y:0*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Л
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskf
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
         ▓
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмє
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Я
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУм_
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:ж
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maskd
MatMulMatMulstrided_slice_1:output:0kernel*
T0*'
_output_shapes
:         <^
MatMul_1MatMulinit_hrecurrent_kernel*
T0*'
_output_shapes
:         <d
addAddV2MatMul:product:0MatMul_1:product:0*
T0*'
_output_shapes
:         <S
BiasAddBiasAddadd:z:0bias*
T0*'
_output_shapes
:         <Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :Х
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_splitT
SigmoidSigmoidsplit:output:0*
T0*'
_output_shapes
:         V
	Sigmoid_1Sigmoidsplit:output:1*
T0*'
_output_shapes
:         S
mulMulSigmoid_1:y:0init_c*
T0*'
_output_shapes
:         N
TanhTanhsplit:output:2*
T0*'
_output_shapes
:         U
mul_1MulSigmoid:y:0Tanh:y:0*
T0*'
_output_shapes
:         T
add_1AddV2mul:z:0	mul_1:z:0*
T0*'
_output_shapes
:         V
	Sigmoid_2Sigmoidsplit:output:3*
T0*'
_output_shapes
:         K
Tanh_1Tanh	add_1:z:0*
T0*'
_output_shapes
:         Y
mul_2MulSigmoid_2:y:0
Tanh_1:y:0*
T0*'
_output_shapes
:         n
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Х
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмF
timeConst*
_output_shapes
: *
dtype0*
value	B : c
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
         T
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : «
whileStatelessWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0init_hinit_cstrided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0kernelrecurrent_kernelbias*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*`
_output_shapesN
L: : : : :         :         : : :<:<:<* 
_read_only_resource_inputs
 *
_stateful_parallelism( *
bodyR
while_body_74040*
condR
while_cond_74039*_
output_shapesN
L: : : : :         :         : : :<:<:<*
parallel_iterations Ђ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       ├
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*,
_output_shapes
:Я         *
element_dtype0h
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
         a
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Є
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          Ќ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*,
_output_shapes
:         Я[
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *  ђ?`
IdentityIdentitystrided_slice_2:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_1:y:0*
T0*,
_output_shapes
:         ЯX

Identity_2Identitywhile:output:4*
T0*'
_output_shapes
:         X

Identity_3Identitywhile:output:5*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_0067de53-6b05-422e-aca0-48144f07387a*
api_preferred_deviceCPU*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
ї	
╝
while_cond_76493
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_76493___redundant_placeholder03
/while_while_cond_76493___redundant_placeholder13
/while_while_cond_76493___redundant_placeholder23
/while_while_cond_76493___redundant_placeholder3
while_identity
`

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: K
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: ")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*S
_input_shapesB
@: : : : :         :         : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
::

_output_shapes
::	

_output_shapes
::


_output_shapes
:
Л@
╦
(__inference_gpu_lstm_with_fallback_74660

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4ѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          v
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :                  P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : o

ExpandDims
ExpandDimsinit_hExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : s
ExpandDims_1
ExpandDimsinit_cExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : Ъ
concat_1ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0*
_output_shapes	
:ЗМ
CudnnRNNCudnnRNNtranspose:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1:output:0*
T0*f
_output_shapesT
R:                  :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          Ё
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*4
_output_shapes"
 :                  p
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         f

Identity_1Identitytranspose_9:y:0*
T0*4
_output_shapes"
 :                  Z

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*s
_input_shapesb
`:                  :         :         :<:<:<*=
api_implements+)lstm_8d4f5c85-cfbf-442f-90ec-8d72d94e1b41*
api_preferred_deviceGPU*
go_backwards( *

time_major( :\ X
4
_output_shapes"
 :                  
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
Н
В
,__inference_sequential_3_layer_call_fn_76421

inputs
unknown:<
	unknown_0:<
	unknown_1:<
	unknown_2:
	unknown_3:
identityѕбStatefulPartitionedCallє
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:         *'
_read_only_resource_inputs	
*0
config_proto 

CPU

GPU2*0J 8ѓ *P
fKRI
G__inference_sequential_3_layer_call_and_return_conditional_losses_76325o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         `
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*5
_input_shapes$
":         Я: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs
├:
┐
__inference_standard_lstm_74566

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4c
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          v
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :                  B
ShapeShapetranspose:y:0*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Л
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskf
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
         ▓
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмє
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Я
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУм_
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:ж
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maskd
MatMulMatMulstrided_slice_1:output:0kernel*
T0*'
_output_shapes
:         <^
MatMul_1MatMulinit_hrecurrent_kernel*
T0*'
_output_shapes
:         <d
addAddV2MatMul:product:0MatMul_1:product:0*
T0*'
_output_shapes
:         <S
BiasAddBiasAddadd:z:0bias*
T0*'
_output_shapes
:         <Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :Х
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_splitT
SigmoidSigmoidsplit:output:0*
T0*'
_output_shapes
:         V
	Sigmoid_1Sigmoidsplit:output:1*
T0*'
_output_shapes
:         S
mulMulSigmoid_1:y:0init_c*
T0*'
_output_shapes
:         N
TanhTanhsplit:output:2*
T0*'
_output_shapes
:         U
mul_1MulSigmoid:y:0Tanh:y:0*
T0*'
_output_shapes
:         T
add_1AddV2mul:z:0	mul_1:z:0*
T0*'
_output_shapes
:         V
	Sigmoid_2Sigmoidsplit:output:3*
T0*'
_output_shapes
:         K
Tanh_1Tanh	add_1:z:0*
T0*'
_output_shapes
:         Y
mul_2MulSigmoid_2:y:0
Tanh_1:y:0*
T0*'
_output_shapes
:         n
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Х
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмF
timeConst*
_output_shapes
: *
dtype0*
value	B : c
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
         T
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : «
whileStatelessWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0init_hinit_cstrided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0kernelrecurrent_kernelbias*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*`
_output_shapesN
L: : : : :         :         : : :<:<:<* 
_read_only_resource_inputs
 *
_stateful_parallelism( *
bodyR
while_body_74481*
condR
while_cond_74480*_
output_shapesN
L: : : : :         :         : : :<:<:<*
parallel_iterations Ђ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       ╦
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :                  *
element_dtype0h
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
         a
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Є
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          Ъ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :                  [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *  ђ?`
IdentityIdentitystrided_slice_2:output:0*
T0*'
_output_shapes
:         f

Identity_1Identitytranspose_1:y:0*
T0*4
_output_shapes"
 :                  X

Identity_2Identitywhile:output:4*
T0*'
_output_shapes
:         X

Identity_3Identitywhile:output:5*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*s
_input_shapesb
`:                  :         :         :<:<:<*=
api_implements+)lstm_8d4f5c85-cfbf-442f-90ec-8d72d94e1b41*
api_preferred_deviceCPU*
go_backwards( *

time_major( :\ X
4
_output_shapes"
 :                  
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
ш
░
&__inference_lstm_3_layer_call_fn_77377

inputs
unknown:<
	unknown_0:<
	unknown_1:<
identityѕбStatefulPartitionedCallТ
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:         *%
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8ѓ *J
fERC
A__inference_lstm_3_layer_call_and_return_conditional_losses_75759o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         `
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*1
_input_shapes 
:         Я: : : 22
StatefulPartitionedCallStatefulPartitionedCall:T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs
ї	
╝
while_cond_74039
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_74039___redundant_placeholder03
/while_while_cond_74039___redundant_placeholder13
/while_while_cond_74039___redundant_placeholder23
/while_while_cond_74039___redundant_placeholder3
while_identity
`

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: K
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: ")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*S
_input_shapesB
@: : : : :         :         : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
::

_output_shapes
::	

_output_shapes
::


_output_shapes
:
Њ:
┐
__inference_standard_lstm_76579

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4c
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          n
	transpose	Transposeinputstranspose/perm:output:0*
T0*,
_output_shapes
:Я         B
ShapeShapetranspose:y:0*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Л
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskf
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
         ▓
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмє
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Я
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУм_
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:ж
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maskd
MatMulMatMulstrided_slice_1:output:0kernel*
T0*'
_output_shapes
:         <^
MatMul_1MatMulinit_hrecurrent_kernel*
T0*'
_output_shapes
:         <d
addAddV2MatMul:product:0MatMul_1:product:0*
T0*'
_output_shapes
:         <S
BiasAddBiasAddadd:z:0bias*
T0*'
_output_shapes
:         <Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :Х
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_splitT
SigmoidSigmoidsplit:output:0*
T0*'
_output_shapes
:         V
	Sigmoid_1Sigmoidsplit:output:1*
T0*'
_output_shapes
:         S
mulMulSigmoid_1:y:0init_c*
T0*'
_output_shapes
:         N
TanhTanhsplit:output:2*
T0*'
_output_shapes
:         U
mul_1MulSigmoid:y:0Tanh:y:0*
T0*'
_output_shapes
:         T
add_1AddV2mul:z:0	mul_1:z:0*
T0*'
_output_shapes
:         V
	Sigmoid_2Sigmoidsplit:output:3*
T0*'
_output_shapes
:         K
Tanh_1Tanh	add_1:z:0*
T0*'
_output_shapes
:         Y
mul_2MulSigmoid_2:y:0
Tanh_1:y:0*
T0*'
_output_shapes
:         n
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Х
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмF
timeConst*
_output_shapes
: *
dtype0*
value	B : c
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
         T
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : «
whileStatelessWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0init_hinit_cstrided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0kernelrecurrent_kernelbias*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*`
_output_shapesN
L: : : : :         :         : : :<:<:<* 
_read_only_resource_inputs
 *
_stateful_parallelism( *
bodyR
while_body_76494*
condR
while_cond_76493*_
output_shapesN
L: : : : :         :         : : :<:<:<*
parallel_iterations Ђ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       ├
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*,
_output_shapes
:Я         *
element_dtype0h
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
         a
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Є
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          Ќ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*,
_output_shapes
:         Я[
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *  ђ?`
IdentityIdentitystrided_slice_2:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_1:y:0*
T0*,
_output_shapes
:         ЯX

Identity_2Identitywhile:output:4*
T0*'
_output_shapes
:         X

Identity_3Identitywhile:output:5*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_a92946bc-3bab-4632-a518-695c88a7828f*
api_preferred_deviceCPU*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
к(
╬
while_body_78818
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0
while_matmul_kernel_0%
!while_matmul_1_recurrent_kernel_0
while_biasadd_bias_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor
while_matmul_kernel#
while_matmul_1_recurrent_kernel
while_biasadd_biasѕ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       д
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:         *
element_dtype0Љ
while/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0while_matmul_kernel_0*
T0*'
_output_shapes
:         <ѓ
while/MatMul_1MatMulwhile_placeholder_2!while_matmul_1_recurrent_kernel_0*
T0*'
_output_shapes
:         <v
	while/addAddV2while/MatMul:product:0while/MatMul_1:product:0*
T0*'
_output_shapes
:         <o
while/BiasAddBiasAddwhile/add:z:0while_biasadd_bias_0*
T0*'
_output_shapes
:         <W
while/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :╚
while/splitSplitwhile/split/split_dim:output:0while/BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_split`
while/SigmoidSigmoidwhile/split:output:0*
T0*'
_output_shapes
:         b
while/Sigmoid_1Sigmoidwhile/split:output:1*
T0*'
_output_shapes
:         l
	while/mulMulwhile/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:         Z

while/TanhTanhwhile/split:output:2*
T0*'
_output_shapes
:         g
while/mul_1Mulwhile/Sigmoid:y:0while/Tanh:y:0*
T0*'
_output_shapes
:         f
while/add_1AddV2while/mul:z:0while/mul_1:z:0*
T0*'
_output_shapes
:         b
while/Sigmoid_2Sigmoidwhile/split:output:3*
T0*'
_output_shapes
:         W
while/Tanh_1Tanhwhile/add_1:z:0*
T0*'
_output_shapes
:         k
while/mul_2Mulwhile/Sigmoid_2:y:0while/Tanh_1:y:0*
T0*'
_output_shapes
:         И
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/mul_2:z:0*
_output_shapes
: *
element_dtype0:жУмO
while/add_2/yConst*
_output_shapes
: *
dtype0*
value	B :`
while/add_2AddV2while_placeholderwhile/add_2/y:output:0*
T0*
_output_shapes
: O
while/add_3/yConst*
_output_shapes
: *
dtype0*
value	B :g
while/add_3AddV2while_while_loop_counterwhile/add_3/y:output:0*
T0*
_output_shapes
: L
while/IdentityIdentitywhile/add_3:z:0*
T0*
_output_shapes
: ]
while/Identity_1Identitywhile_while_maximum_iterations*
T0*
_output_shapes
: N
while/Identity_2Identitywhile/add_2:z:0*
T0*
_output_shapes
: ї
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: :жУм_
while/Identity_4Identitywhile/mul_2:z:0*
T0*'
_output_shapes
:         _
while/Identity_5Identitywhile/add_1:z:0*
T0*'
_output_shapes
:         "*
while_biasadd_biaswhile_biasadd_bias_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"D
while_matmul_1_recurrent_kernel!while_matmul_1_recurrent_kernel_0",
while_matmul_kernelwhile_matmul_kernel_0",
while_strided_slicewhile_strided_slice_0"е
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*_
_input_shapesN
L: : : : :         :         : : :<:<:<: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:<:$	 

_output_shapes

:<: 


_output_shapes
:<
ї	
╝
while_cond_75924
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_75924___redundant_placeholder03
/while_while_cond_75924___redundant_placeholder13
/while_while_cond_75924___redundant_placeholder23
/while_while_cond_75924___redundant_placeholder3
while_identity
`

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: K
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: ")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*S
_input_shapesB
@: : : : :         :         : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
::

_output_shapes
::	

_output_shapes
::


_output_shapes
:
▓
├
G__inference_sequential_3_layer_call_and_return_conditional_losses_76325

inputs
lstm_3_76312:<
lstm_3_76314:<
lstm_3_76316:<
dense_3_76319:
dense_3_76321:
identityѕбdense_3/StatefulPartitionedCallбlstm_3/StatefulPartitionedCallЭ
lstm_3/StatefulPartitionedCallStatefulPartitionedCallinputslstm_3_76312lstm_3_76314lstm_3_76316*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:         *%
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8ѓ *J
fERC
A__inference_lstm_3_layer_call_and_return_conditional_losses_76283Ї
dense_3/StatefulPartitionedCallStatefulPartitionedCall'lstm_3/StatefulPartitionedCall:output:0dense_3_76319dense_3_76321*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:         *$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8ѓ *K
fFRD
B__inference_dense_3_layer_call_and_return_conditional_losses_75777w
IdentityIdentity(dense_3/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         Ѕ
NoOpNoOp ^dense_3/StatefulPartitionedCall^lstm_3/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*5
_input_shapes$
":         Я: : : : : 2B
dense_3/StatefulPartitionedCalldense_3/StatefulPartitionedCall2@
lstm_3/StatefulPartitionedCalllstm_3/StatefulPartitionedCall:T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs
Ж┴
ы
9__inference___backward_gpu_lstm_with_fallback_77143_77319
placeholder
placeholder_1
placeholder_2
placeholder_3
placeholder_4/
+gradients_strided_slice_grad_shape_cudnnrnnA
=gradients_transpose_9_grad_invertpermutation_transpose_9_perm)
%gradients_squeeze_grad_shape_cudnnrnn+
'gradients_squeeze_1_grad_shape_cudnnrnn!
gradients_zeros_like_cudnnrnn6
2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose7
3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims9
5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_15
1gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1=
9gradients_transpose_grad_invertpermutation_transpose_perm*
&gradients_expanddims_grad_shape_init_h,
(gradients_expanddims_1_grad_shape_init_c-
)gradients_concat_1_grad_mod_concat_1_axisA
=gradients_transpose_1_grad_invertpermutation_transpose_1_permA
=gradients_transpose_2_grad_invertpermutation_transpose_2_permA
=gradients_transpose_3_grad_invertpermutation_transpose_3_permA
=gradients_transpose_4_grad_invertpermutation_transpose_4_permA
=gradients_transpose_5_grad_invertpermutation_transpose_5_permA
=gradients_transpose_6_grad_invertpermutation_transpose_6_permA
=gradients_transpose_7_grad_invertpermutation_transpose_7_permA
=gradients_transpose_8_grad_invertpermutation_transpose_8_perm3
/gradients_split_2_grad_concat_split_2_split_dim/
+gradients_split_grad_concat_split_split_dim3
/gradients_split_1_grad_concat_split_1_split_dim)
%gradients_concat_grad_mod_concat_axis
identity

identity_1

identity_2

identity_3

identity_4

identity_5ѕ^
gradients/grad_ys_0Identityplaceholder*
T0*'
_output_shapes
:         e
gradients/grad_ys_1Identityplaceholder_1*
T0*,
_output_shapes
:         Я`
gradients/grad_ys_2Identityplaceholder_2*
T0*'
_output_shapes
:         `
gradients/grad_ys_3Identityplaceholder_3*
T0*'
_output_shapes
:         O
gradients/grad_ys_4Identityplaceholder_4*
T0*
_output_shapes
: }
"gradients/strided_slice_grad/ShapeShape+gradients_strided_slice_grad_shape_cudnnrnn*
T0*
_output_shapes
:є
3gradients/strided_slice_grad/StridedSliceGrad/beginConst*
_output_shapes
:*
dtype0*
valueB:
         {
1gradients/strided_slice_grad/StridedSliceGrad/endConst*
_output_shapes
:*
dtype0*
valueB: 
5gradients/strided_slice_grad/StridedSliceGrad/stridesConst*
_output_shapes
:*
dtype0*
valueB:б
-gradients/strided_slice_grad/StridedSliceGradStridedSliceGrad+gradients/strided_slice_grad/Shape:output:0<gradients/strided_slice_grad/StridedSliceGrad/begin:output:0:gradients/strided_slice_grad/StridedSliceGrad/end:output:0>gradients/strided_slice_grad/StridedSliceGrad/strides:output:0gradients/grad_ys_0:output:0*
Index0*
T0*,
_output_shapes
:Я         *
shrink_axis_maskю
,gradients/transpose_9_grad/InvertPermutationInvertPermutation=gradients_transpose_9_grad_invertpermutation_transpose_9_perm*
_output_shapes
:И
$gradients/transpose_9_grad/transpose	Transposegradients/grad_ys_1:output:00gradients/transpose_9_grad/InvertPermutation:y:0*
T0*,
_output_shapes
:Я         q
gradients/Squeeze_grad/ShapeShape%gradients_squeeze_grad_shape_cudnnrnn*
T0*
_output_shapes
:ц
gradients/Squeeze_grad/ReshapeReshapegradients/grad_ys_2:output:0%gradients/Squeeze_grad/Shape:output:0*
T0*+
_output_shapes
:         u
gradients/Squeeze_1_grad/ShapeShape'gradients_squeeze_1_grad_shape_cudnnrnn*
T0*
_output_shapes
:е
 gradients/Squeeze_1_grad/ReshapeReshapegradients/grad_ys_3:output:0'gradients/Squeeze_1_grad/Shape:output:0*
T0*+
_output_shapes
:         Щ
gradients/AddNAddN6gradients/strided_slice_grad/StridedSliceGrad:output:0(gradients/transpose_9_grad/transpose:y:0*
N*
T0*@
_class6
42loc:@gradients/strided_slice_grad/StridedSliceGrad*,
_output_shapes
:Я         c
gradients/zeros_like	ZerosLikegradients_zeros_like_cudnnrnn*
T0*
_output_shapes
:Ђ
(gradients/CudnnRNN_grad/CudnnRNNBackpropCudnnRNNBackprop2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_11gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1+gradients_strided_slice_grad_shape_cudnnrnn%gradients_squeeze_grad_shape_cudnnrnn'gradients_squeeze_1_grad_shape_cudnnrnngradients/AddN:sum:0'gradients/Squeeze_grad/Reshape:output:0)gradients/Squeeze_1_grad/Reshape:output:0gradients_zeros_like_cudnnrnn*
T0*a
_output_shapesO
M:Я         :         :         :Зќ
*gradients/transpose_grad/InvertPermutationInvertPermutation9gradients_transpose_grad_invertpermutation_transpose_perm*
_output_shapes
:Л
"gradients/transpose_grad/transpose	Transpose9gradients/CudnnRNN_grad/CudnnRNNBackprop:input_backprop:0.gradients/transpose_grad/InvertPermutation:y:0*
T0*,
_output_shapes
:         Яu
gradients/ExpandDims_grad/ShapeShape&gradients_expanddims_grad_shape_init_h*
T0*
_output_shapes
:┼
!gradients/ExpandDims_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_h_backprop:0(gradients/ExpandDims_grad/Shape:output:0*
T0*'
_output_shapes
:         y
!gradients/ExpandDims_1_grad/ShapeShape(gradients_expanddims_1_grad_shape_init_c*
T0*
_output_shapes
:╔
#gradients/ExpandDims_1_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_c_backprop:0*gradients/ExpandDims_1_grad/Shape:output:0*
T0*'
_output_shapes
:         ^
gradients/concat_1_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :џ
gradients/concat_1_grad/modFloorMod)gradients_concat_1_grad_mod_concat_1_axis%gradients/concat_1_grad/Rank:output:0*
T0*
_output_shapes
: g
gradients/concat_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_2Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_3Const*
_output_shapes
:*
dtype0*
valueB:j
gradients/concat_1_grad/Shape_4Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_5Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_6Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_7Const*
_output_shapes
:*
dtype0*
valueB:рi
gradients/concat_1_grad/Shape_8Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_9Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_10Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_11Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_12Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_13Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_14Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_15Const*
_output_shapes
:*
dtype0*
valueB:Э
$gradients/concat_1_grad/ConcatOffsetConcatOffsetgradients/concat_1_grad/mod:z:0&gradients/concat_1_grad/Shape:output:0(gradients/concat_1_grad/Shape_1:output:0(gradients/concat_1_grad/Shape_2:output:0(gradients/concat_1_grad/Shape_3:output:0(gradients/concat_1_grad/Shape_4:output:0(gradients/concat_1_grad/Shape_5:output:0(gradients/concat_1_grad/Shape_6:output:0(gradients/concat_1_grad/Shape_7:output:0(gradients/concat_1_grad/Shape_8:output:0(gradients/concat_1_grad/Shape_9:output:0)gradients/concat_1_grad/Shape_10:output:0)gradients/concat_1_grad/Shape_11:output:0)gradients/concat_1_grad/Shape_12:output:0)gradients/concat_1_grad/Shape_13:output:0)gradients/concat_1_grad/Shape_14:output:0)gradients/concat_1_grad/Shape_15:output:0*
N*t
_output_shapesb
`::::::::::::::::в
gradients/concat_1_grad/SliceSlice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:0&gradients/concat_1_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_1Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:1(gradients/concat_1_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_2Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:2(gradients/concat_1_grad/Shape_2:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_3Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:3(gradients/concat_1_grad/Shape_3:output:0*
Index0*
T0*
_output_shapes
:­
gradients/concat_1_grad/Slice_4Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:4(gradients/concat_1_grad/Shape_4:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_5Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:5(gradients/concat_1_grad/Shape_5:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_6Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:6(gradients/concat_1_grad/Shape_6:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_7Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:7(gradients/concat_1_grad/Shape_7:output:0*
Index0*
T0*
_output_shapes	
:р№
gradients/concat_1_grad/Slice_8Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:8(gradients/concat_1_grad/Shape_8:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_9Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:9(gradients/concat_1_grad/Shape_9:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_10Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:10)gradients/concat_1_grad/Shape_10:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_11Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:11)gradients/concat_1_grad/Shape_11:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_12Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:12)gradients/concat_1_grad/Shape_12:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_13Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:13)gradients/concat_1_grad/Shape_13:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_14Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:14)gradients/concat_1_grad/Shape_14:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_15Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:15)gradients/concat_1_grad/Shape_15:output:0*
Index0*
T0*
_output_shapes
:m
gradients/Reshape_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      А
gradients/Reshape_grad/ReshapeReshape&gradients/concat_1_grad/Slice:output:0%gradients/Reshape_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_1_grad/ReshapeReshape(gradients/concat_1_grad/Slice_1:output:0'gradients/Reshape_1_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_2_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_2_grad/ReshapeReshape(gradients/concat_1_grad/Slice_2:output:0'gradients/Reshape_2_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_3_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_3_grad/ReshapeReshape(gradients/concat_1_grad/Slice_3:output:0'gradients/Reshape_3_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_4_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_4_grad/ReshapeReshape(gradients/concat_1_grad/Slice_4:output:0'gradients/Reshape_4_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_5_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_5_grad/ReshapeReshape(gradients/concat_1_grad/Slice_5:output:0'gradients/Reshape_5_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_6_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_6_grad/ReshapeReshape(gradients/concat_1_grad/Slice_6:output:0'gradients/Reshape_6_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_7_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_7_grad/ReshapeReshape(gradients/concat_1_grad/Slice_7:output:0'gradients/Reshape_7_grad/Shape:output:0*
T0*
_output_shapes

:h
gradients/Reshape_8_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_8_grad/ReshapeReshape(gradients/concat_1_grad/Slice_8:output:0'gradients/Reshape_8_grad/Shape:output:0*
T0*
_output_shapes
:h
gradients/Reshape_9_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_9_grad/ReshapeReshape(gradients/concat_1_grad/Slice_9:output:0'gradients/Reshape_9_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_10_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_10_grad/ReshapeReshape)gradients/concat_1_grad/Slice_10:output:0(gradients/Reshape_10_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_11_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_11_grad/ReshapeReshape)gradients/concat_1_grad/Slice_11:output:0(gradients/Reshape_11_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_12_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_12_grad/ReshapeReshape)gradients/concat_1_grad/Slice_12:output:0(gradients/Reshape_12_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_13_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_13_grad/ReshapeReshape)gradients/concat_1_grad/Slice_13:output:0(gradients/Reshape_13_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_14_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_14_grad/ReshapeReshape)gradients/concat_1_grad/Slice_14:output:0(gradients/Reshape_14_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_15_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_15_grad/ReshapeReshape)gradients/concat_1_grad/Slice_15:output:0(gradients/Reshape_15_grad/Shape:output:0*
T0*
_output_shapes
:ю
,gradients/transpose_1_grad/InvertPermutationInvertPermutation=gradients_transpose_1_grad_invertpermutation_transpose_1_perm*
_output_shapes
:х
$gradients/transpose_1_grad/transpose	Transpose'gradients/Reshape_grad/Reshape:output:00gradients/transpose_1_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_2_grad/InvertPermutationInvertPermutation=gradients_transpose_2_grad_invertpermutation_transpose_2_perm*
_output_shapes
:и
$gradients/transpose_2_grad/transpose	Transpose)gradients/Reshape_1_grad/Reshape:output:00gradients/transpose_2_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_3_grad/InvertPermutationInvertPermutation=gradients_transpose_3_grad_invertpermutation_transpose_3_perm*
_output_shapes
:и
$gradients/transpose_3_grad/transpose	Transpose)gradients/Reshape_2_grad/Reshape:output:00gradients/transpose_3_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_4_grad/InvertPermutationInvertPermutation=gradients_transpose_4_grad_invertpermutation_transpose_4_perm*
_output_shapes
:и
$gradients/transpose_4_grad/transpose	Transpose)gradients/Reshape_3_grad/Reshape:output:00gradients/transpose_4_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_5_grad/InvertPermutationInvertPermutation=gradients_transpose_5_grad_invertpermutation_transpose_5_perm*
_output_shapes
:и
$gradients/transpose_5_grad/transpose	Transpose)gradients/Reshape_4_grad/Reshape:output:00gradients/transpose_5_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_6_grad/InvertPermutationInvertPermutation=gradients_transpose_6_grad_invertpermutation_transpose_6_perm*
_output_shapes
:и
$gradients/transpose_6_grad/transpose	Transpose)gradients/Reshape_5_grad/Reshape:output:00gradients/transpose_6_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_7_grad/InvertPermutationInvertPermutation=gradients_transpose_7_grad_invertpermutation_transpose_7_perm*
_output_shapes
:и
$gradients/transpose_7_grad/transpose	Transpose)gradients/Reshape_6_grad/Reshape:output:00gradients/transpose_7_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_8_grad/InvertPermutationInvertPermutation=gradients_transpose_8_grad_invertpermutation_transpose_8_perm*
_output_shapes
:и
$gradients/transpose_8_grad/transpose	Transpose)gradients/Reshape_7_grad/Reshape:output:00gradients/transpose_8_grad/InvertPermutation:y:0*
T0*
_output_shapes

:Т
gradients/split_2_grad/concatConcatV2)gradients/Reshape_8_grad/Reshape:output:0)gradients/Reshape_9_grad/Reshape:output:0*gradients/Reshape_10_grad/Reshape:output:0*gradients/Reshape_11_grad/Reshape:output:0*gradients/Reshape_12_grad/Reshape:output:0*gradients/Reshape_13_grad/Reshape:output:0*gradients/Reshape_14_grad/Reshape:output:0*gradients/Reshape_15_grad/Reshape:output:0/gradients_split_2_grad_concat_split_2_split_dim*
N*
T0*
_output_shapes
:x«
gradients/split_grad/concatConcatV2(gradients/transpose_1_grad/transpose:y:0(gradients/transpose_2_grad/transpose:y:0(gradients/transpose_3_grad/transpose:y:0(gradients/transpose_4_grad/transpose:y:0+gradients_split_grad_concat_split_split_dim*
N*
T0*
_output_shapes

:<┤
gradients/split_1_grad/concatConcatV2(gradients/transpose_5_grad/transpose:y:0(gradients/transpose_6_grad/transpose:y:0(gradients/transpose_7_grad/transpose:y:0(gradients/transpose_8_grad/transpose:y:0/gradients_split_1_grad_concat_split_1_split_dim*
N*
T0*
_output_shapes

:<\
gradients/concat_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :њ
gradients/concat_grad/modFloorMod%gradients_concat_grad_mod_concat_axis#gradients/concat_grad/Rank:output:0*
T0*
_output_shapes
: e
gradients/concat_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:<g
gradients/concat_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:<╩
"gradients/concat_grad/ConcatOffsetConcatOffsetgradients/concat_grad/mod:z:0$gradients/concat_grad/Shape:output:0&gradients/concat_grad/Shape_1:output:0*
N* 
_output_shapes
::Л
gradients/concat_grad/SliceSlice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:0$gradients/concat_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:<Н
gradients/concat_grad/Slice_1Slice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:1&gradients/concat_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:<s
IdentityIdentity&gradients/transpose_grad/transpose:y:0*
T0*,
_output_shapes
:         Яt

Identity_1Identity*gradients/ExpandDims_grad/Reshape:output:0*
T0*'
_output_shapes
:         v

Identity_2Identity,gradients/ExpandDims_1_grad/Reshape:output:0*
T0*'
_output_shapes
:         e

Identity_3Identity$gradients/split_grad/concat:output:0*
T0*
_output_shapes

:<g

Identity_4Identity&gradients/split_1_grad/concat:output:0*
T0*
_output_shapes

:<c

Identity_5Identity&gradients/concat_grad/Slice_1:output:0*
T0*
_output_shapes
:<"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"!

identity_5Identity_5:output:0*(
_construction_contextkEagerRuntime*в
_input_shapes┘
о:         :         Я:         :         : :Я         ::         :         ::Я         :         :         :З::         :         : ::::::::: : : : *=
api_implements+)lstm_91ae4c3a-1bb4-4840-80b7-ab1926c68132*
api_preferred_deviceGPU*A
forward_function_name(&__forward_gpu_lstm_with_fallback_77318*
go_backwards( *

time_major( :- )
'
_output_shapes
:         :2.
,
_output_shapes
:         Я:-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :2.
,
_output_shapes
:Я         : 

_output_shapes
::1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :	

_output_shapes
::2
.
,
_output_shapes
:Я         :1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :!

_output_shapes	
:З: 

_output_shapes
::-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: : 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
::

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
у:
И
A__inference_lstm_3_layer_call_and_return_conditional_losses_79176

inputs.
read_readvariableop_resource:<0
read_1_readvariableop_resource:<,
read_2_readvariableop_resource:<

identity_3ѕбRead/ReadVariableOpбRead_1/ReadVariableOpбRead_2/ReadVariableOp;
ShapeShapeinputs*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Л
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskP
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :s
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:P
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    l
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:         R
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :w
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:R
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    r
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:         E
ones_like/ShapeShapeinputs*
T0*
_output_shapes
:T
ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ђ?|
	ones_likeFillones_like/Shape:output:0ones_like/Const:output:0*
T0*,
_output_shapes
:         ЯR
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?u
dropout/MulMulones_like:output:0dropout/Const:output:0*
T0*,
_output_shapes
:         ЯO
dropout/ShapeShapeones_like:output:0*
T0*
_output_shapes
:Љ
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*,
_output_shapes
:         Я*
dtype0[
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<Ф
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*,
_output_shapes
:         Яt
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*,
_output_shapes
:         Яn
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*,
_output_shapes
:         ЯT
dropout_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?y
dropout_1/MulMulones_like:output:0dropout_1/Const:output:0*
T0*,
_output_shapes
:         ЯQ
dropout_1/ShapeShapeones_like:output:0*
T0*
_output_shapes
:Ћ
&dropout_1/random_uniform/RandomUniformRandomUniformdropout_1/Shape:output:0*
T0*,
_output_shapes
:         Я*
dtype0]
dropout_1/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<▒
dropout_1/GreaterEqualGreaterEqual/dropout_1/random_uniform/RandomUniform:output:0!dropout_1/GreaterEqual/y:output:0*
T0*,
_output_shapes
:         Яx
dropout_1/CastCastdropout_1/GreaterEqual:z:0*

DstT0*

SrcT0
*,
_output_shapes
:         Яt
dropout_1/Mul_1Muldropout_1/Mul:z:0dropout_1/Cast:y:0*
T0*,
_output_shapes
:         ЯT
dropout_2/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?y
dropout_2/MulMulones_like:output:0dropout_2/Const:output:0*
T0*,
_output_shapes
:         ЯQ
dropout_2/ShapeShapeones_like:output:0*
T0*
_output_shapes
:Ћ
&dropout_2/random_uniform/RandomUniformRandomUniformdropout_2/Shape:output:0*
T0*,
_output_shapes
:         Я*
dtype0]
dropout_2/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<▒
dropout_2/GreaterEqualGreaterEqual/dropout_2/random_uniform/RandomUniform:output:0!dropout_2/GreaterEqual/y:output:0*
T0*,
_output_shapes
:         Яx
dropout_2/CastCastdropout_2/GreaterEqual:z:0*

DstT0*

SrcT0
*,
_output_shapes
:         Яt
dropout_2/Mul_1Muldropout_2/Mul:z:0dropout_2/Cast:y:0*
T0*,
_output_shapes
:         ЯT
dropout_3/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *§JЂ?y
dropout_3/MulMulones_like:output:0dropout_3/Const:output:0*
T0*,
_output_shapes
:         ЯQ
dropout_3/ShapeShapeones_like:output:0*
T0*
_output_shapes
:Ћ
&dropout_3/random_uniform/RandomUniformRandomUniformdropout_3/Shape:output:0*
T0*,
_output_shapes
:         Я*
dtype0]
dropout_3/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *
О#<▒
dropout_3/GreaterEqualGreaterEqual/dropout_3/random_uniform/RandomUniform:output:0!dropout_3/GreaterEqual/y:output:0*
T0*,
_output_shapes
:         Яx
dropout_3/CastCastdropout_3/GreaterEqual:z:0*

DstT0*

SrcT0
*,
_output_shapes
:         Яt
dropout_3/Mul_1Muldropout_3/Mul:z:0dropout_3/Cast:y:0*
T0*,
_output_shapes
:         Я\
mulMulinputsdropout/Mul_1:z:0*
T0*,
_output_shapes
:         Яp
Read/ReadVariableOpReadVariableOpread_readvariableop_resource*
_output_shapes

:<*
dtype0Z
IdentityIdentityRead/ReadVariableOp:value:0*
T0*
_output_shapes

:<t
Read_1/ReadVariableOpReadVariableOpread_1_readvariableop_resource*
_output_shapes

:<*
dtype0^

Identity_1IdentityRead_1/ReadVariableOp:value:0*
T0*
_output_shapes

:<p
Read_2/ReadVariableOpReadVariableOpread_2_readvariableop_resource*
_output_shapes
:<*
dtype0Z

Identity_2IdentityRead_2/ReadVariableOp:value:0*
T0*
_output_shapes
:<Х
PartitionedCallPartitionedCallmul:z:0zeros:output:0zeros_1:output:0Identity:output:0Identity_1:output:0Identity_2:output:0*
Tin

2*
Tout	
2*
_collective_manager_ids
 *g
_output_shapesU
S:         :         Я:         :         : * 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8ѓ *(
f#R!
__inference_standard_lstm_78903i

Identity_3IdentityPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         ї
NoOpNoOp^Read/ReadVariableOp^Read_1/ReadVariableOp^Read_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*1
_input_shapes 
:         Я: : : 2*
Read/ReadVariableOpRead/ReadVariableOp2.
Read_1/ReadVariableOpRead_1/ReadVariableOp2.
Read_2/ReadVariableOpRead_2/ReadVariableOp:T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs
а@
╦
(__inference_gpu_lstm_with_fallback_78997

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4ѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          n
	transpose	Transposeinputstranspose/perm:output:0*
T0*,
_output_shapes
:Я         P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : o

ExpandDims
ExpandDimsinit_hExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : s
ExpandDims_1
ExpandDimsinit_cExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : Ъ
concat_1ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0*
_output_shapes	
:З╦
CudnnRNNCudnnRNNtranspose:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1:output:0*
T0*^
_output_shapesL
J:Я         :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          }
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*,
_output_shapes
:         Яp
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_9:y:0*
T0*,
_output_shapes
:         ЯZ

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_428e9d17-ff82-4115-bdf7-a5882938e408*
api_preferred_deviceGPU*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
и
ж
#__inference_signature_wrapper_77344
lstm_3_input
unknown:<
	unknown_0:<
	unknown_1:<
	unknown_2:
	unknown_3:
identityѕбStatefulPartitionedCallт
StatefulPartitionedCallStatefulPartitionedCalllstm_3_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:         *'
_read_only_resource_inputs	
*0
config_proto 

CPU

GPU2*0J 8ѓ *)
f$R"
 __inference__wrapped_model_74404o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         `
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*5
_input_shapes$
":         Я: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:Z V
,
_output_shapes
:         Я
&
_user_specified_namelstm_3_input
у
Ы
,__inference_sequential_3_layer_call_fn_76353
lstm_3_input
unknown:<
	unknown_0:<
	unknown_1:<
	unknown_2:
	unknown_3:
identityѕбStatefulPartitionedCallї
StatefulPartitionedCallStatefulPartitionedCalllstm_3_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:         *'
_read_only_resource_inputs	
*0
config_proto 

CPU

GPU2*0J 8ѓ *P
fKRI
G__inference_sequential_3_layer_call_and_return_conditional_losses_76325o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         `
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*5
_input_shapes$
":         Я: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:Z V
,
_output_shapes
:         Я
&
_user_specified_namelstm_3_input
ї	
╝
while_cond_74480
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_74480___redundant_placeholder03
/while_while_cond_74480___redundant_placeholder13
/while_while_cond_74480___redundant_placeholder23
/while_while_cond_74480___redundant_placeholder3
while_identity
`

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: K
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: ")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*S
_input_shapesB
@: : : : :         :         : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
::

_output_shapes
::	

_output_shapes
::


_output_shapes
:
м┬
ы
9__inference___backward_gpu_lstm_with_fallback_74661_74837
placeholder
placeholder_1
placeholder_2
placeholder_3
placeholder_4/
+gradients_strided_slice_grad_shape_cudnnrnnA
=gradients_transpose_9_grad_invertpermutation_transpose_9_perm)
%gradients_squeeze_grad_shape_cudnnrnn+
'gradients_squeeze_1_grad_shape_cudnnrnn!
gradients_zeros_like_cudnnrnn6
2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose7
3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims9
5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_15
1gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1=
9gradients_transpose_grad_invertpermutation_transpose_perm*
&gradients_expanddims_grad_shape_init_h,
(gradients_expanddims_1_grad_shape_init_c-
)gradients_concat_1_grad_mod_concat_1_axisA
=gradients_transpose_1_grad_invertpermutation_transpose_1_permA
=gradients_transpose_2_grad_invertpermutation_transpose_2_permA
=gradients_transpose_3_grad_invertpermutation_transpose_3_permA
=gradients_transpose_4_grad_invertpermutation_transpose_4_permA
=gradients_transpose_5_grad_invertpermutation_transpose_5_permA
=gradients_transpose_6_grad_invertpermutation_transpose_6_permA
=gradients_transpose_7_grad_invertpermutation_transpose_7_permA
=gradients_transpose_8_grad_invertpermutation_transpose_8_perm3
/gradients_split_2_grad_concat_split_2_split_dim/
+gradients_split_grad_concat_split_split_dim3
/gradients_split_1_grad_concat_split_1_split_dim)
%gradients_concat_grad_mod_concat_axis
identity

identity_1

identity_2

identity_3

identity_4

identity_5ѕ^
gradients/grad_ys_0Identityplaceholder*
T0*'
_output_shapes
:         m
gradients/grad_ys_1Identityplaceholder_1*
T0*4
_output_shapes"
 :                  `
gradients/grad_ys_2Identityplaceholder_2*
T0*'
_output_shapes
:         `
gradients/grad_ys_3Identityplaceholder_3*
T0*'
_output_shapes
:         O
gradients/grad_ys_4Identityplaceholder_4*
T0*
_output_shapes
: }
"gradients/strided_slice_grad/ShapeShape+gradients_strided_slice_grad_shape_cudnnrnn*
T0*
_output_shapes
:є
3gradients/strided_slice_grad/StridedSliceGrad/beginConst*
_output_shapes
:*
dtype0*
valueB:
         {
1gradients/strided_slice_grad/StridedSliceGrad/endConst*
_output_shapes
:*
dtype0*
valueB: 
5gradients/strided_slice_grad/StridedSliceGrad/stridesConst*
_output_shapes
:*
dtype0*
valueB:ф
-gradients/strided_slice_grad/StridedSliceGradStridedSliceGrad+gradients/strided_slice_grad/Shape:output:0<gradients/strided_slice_grad/StridedSliceGrad/begin:output:0:gradients/strided_slice_grad/StridedSliceGrad/end:output:0>gradients/strided_slice_grad/StridedSliceGrad/strides:output:0gradients/grad_ys_0:output:0*
Index0*
T0*4
_output_shapes"
 :                  *
shrink_axis_maskю
,gradients/transpose_9_grad/InvertPermutationInvertPermutation=gradients_transpose_9_grad_invertpermutation_transpose_9_perm*
_output_shapes
:└
$gradients/transpose_9_grad/transpose	Transposegradients/grad_ys_1:output:00gradients/transpose_9_grad/InvertPermutation:y:0*
T0*4
_output_shapes"
 :                  q
gradients/Squeeze_grad/ShapeShape%gradients_squeeze_grad_shape_cudnnrnn*
T0*
_output_shapes
:ц
gradients/Squeeze_grad/ReshapeReshapegradients/grad_ys_2:output:0%gradients/Squeeze_grad/Shape:output:0*
T0*+
_output_shapes
:         u
gradients/Squeeze_1_grad/ShapeShape'gradients_squeeze_1_grad_shape_cudnnrnn*
T0*
_output_shapes
:е
 gradients/Squeeze_1_grad/ReshapeReshapegradients/grad_ys_3:output:0'gradients/Squeeze_1_grad/Shape:output:0*
T0*+
_output_shapes
:         ѓ
gradients/AddNAddN6gradients/strided_slice_grad/StridedSliceGrad:output:0(gradients/transpose_9_grad/transpose:y:0*
N*
T0*@
_class6
42loc:@gradients/strided_slice_grad/StridedSliceGrad*4
_output_shapes"
 :                  c
gradients/zeros_like	ZerosLikegradients_zeros_like_cudnnrnn*
T0*
_output_shapes
:Ѕ
(gradients/CudnnRNN_grad/CudnnRNNBackpropCudnnRNNBackprop2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_11gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1+gradients_strided_slice_grad_shape_cudnnrnn%gradients_squeeze_grad_shape_cudnnrnn'gradients_squeeze_1_grad_shape_cudnnrnngradients/AddN:sum:0'gradients/Squeeze_grad/Reshape:output:0)gradients/Squeeze_1_grad/Reshape:output:0gradients_zeros_like_cudnnrnn*
T0*i
_output_shapesW
U:                  :         :         :Зќ
*gradients/transpose_grad/InvertPermutationInvertPermutation9gradients_transpose_grad_invertpermutation_transpose_perm*
_output_shapes
:┘
"gradients/transpose_grad/transpose	Transpose9gradients/CudnnRNN_grad/CudnnRNNBackprop:input_backprop:0.gradients/transpose_grad/InvertPermutation:y:0*
T0*4
_output_shapes"
 :                  u
gradients/ExpandDims_grad/ShapeShape&gradients_expanddims_grad_shape_init_h*
T0*
_output_shapes
:┼
!gradients/ExpandDims_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_h_backprop:0(gradients/ExpandDims_grad/Shape:output:0*
T0*'
_output_shapes
:         y
!gradients/ExpandDims_1_grad/ShapeShape(gradients_expanddims_1_grad_shape_init_c*
T0*
_output_shapes
:╔
#gradients/ExpandDims_1_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_c_backprop:0*gradients/ExpandDims_1_grad/Shape:output:0*
T0*'
_output_shapes
:         ^
gradients/concat_1_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :џ
gradients/concat_1_grad/modFloorMod)gradients_concat_1_grad_mod_concat_1_axis%gradients/concat_1_grad/Rank:output:0*
T0*
_output_shapes
: g
gradients/concat_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_2Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_3Const*
_output_shapes
:*
dtype0*
valueB:j
gradients/concat_1_grad/Shape_4Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_5Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_6Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_7Const*
_output_shapes
:*
dtype0*
valueB:рi
gradients/concat_1_grad/Shape_8Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_9Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_10Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_11Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_12Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_13Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_14Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_15Const*
_output_shapes
:*
dtype0*
valueB:Э
$gradients/concat_1_grad/ConcatOffsetConcatOffsetgradients/concat_1_grad/mod:z:0&gradients/concat_1_grad/Shape:output:0(gradients/concat_1_grad/Shape_1:output:0(gradients/concat_1_grad/Shape_2:output:0(gradients/concat_1_grad/Shape_3:output:0(gradients/concat_1_grad/Shape_4:output:0(gradients/concat_1_grad/Shape_5:output:0(gradients/concat_1_grad/Shape_6:output:0(gradients/concat_1_grad/Shape_7:output:0(gradients/concat_1_grad/Shape_8:output:0(gradients/concat_1_grad/Shape_9:output:0)gradients/concat_1_grad/Shape_10:output:0)gradients/concat_1_grad/Shape_11:output:0)gradients/concat_1_grad/Shape_12:output:0)gradients/concat_1_grad/Shape_13:output:0)gradients/concat_1_grad/Shape_14:output:0)gradients/concat_1_grad/Shape_15:output:0*
N*t
_output_shapesb
`::::::::::::::::в
gradients/concat_1_grad/SliceSlice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:0&gradients/concat_1_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_1Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:1(gradients/concat_1_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_2Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:2(gradients/concat_1_grad/Shape_2:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_3Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:3(gradients/concat_1_grad/Shape_3:output:0*
Index0*
T0*
_output_shapes
:­
gradients/concat_1_grad/Slice_4Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:4(gradients/concat_1_grad/Shape_4:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_5Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:5(gradients/concat_1_grad/Shape_5:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_6Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:6(gradients/concat_1_grad/Shape_6:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_7Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:7(gradients/concat_1_grad/Shape_7:output:0*
Index0*
T0*
_output_shapes	
:р№
gradients/concat_1_grad/Slice_8Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:8(gradients/concat_1_grad/Shape_8:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_9Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:9(gradients/concat_1_grad/Shape_9:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_10Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:10)gradients/concat_1_grad/Shape_10:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_11Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:11)gradients/concat_1_grad/Shape_11:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_12Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:12)gradients/concat_1_grad/Shape_12:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_13Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:13)gradients/concat_1_grad/Shape_13:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_14Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:14)gradients/concat_1_grad/Shape_14:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_15Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:15)gradients/concat_1_grad/Shape_15:output:0*
Index0*
T0*
_output_shapes
:m
gradients/Reshape_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      А
gradients/Reshape_grad/ReshapeReshape&gradients/concat_1_grad/Slice:output:0%gradients/Reshape_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_1_grad/ReshapeReshape(gradients/concat_1_grad/Slice_1:output:0'gradients/Reshape_1_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_2_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_2_grad/ReshapeReshape(gradients/concat_1_grad/Slice_2:output:0'gradients/Reshape_2_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_3_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_3_grad/ReshapeReshape(gradients/concat_1_grad/Slice_3:output:0'gradients/Reshape_3_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_4_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_4_grad/ReshapeReshape(gradients/concat_1_grad/Slice_4:output:0'gradients/Reshape_4_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_5_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_5_grad/ReshapeReshape(gradients/concat_1_grad/Slice_5:output:0'gradients/Reshape_5_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_6_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_6_grad/ReshapeReshape(gradients/concat_1_grad/Slice_6:output:0'gradients/Reshape_6_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_7_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_7_grad/ReshapeReshape(gradients/concat_1_grad/Slice_7:output:0'gradients/Reshape_7_grad/Shape:output:0*
T0*
_output_shapes

:h
gradients/Reshape_8_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_8_grad/ReshapeReshape(gradients/concat_1_grad/Slice_8:output:0'gradients/Reshape_8_grad/Shape:output:0*
T0*
_output_shapes
:h
gradients/Reshape_9_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_9_grad/ReshapeReshape(gradients/concat_1_grad/Slice_9:output:0'gradients/Reshape_9_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_10_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_10_grad/ReshapeReshape)gradients/concat_1_grad/Slice_10:output:0(gradients/Reshape_10_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_11_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_11_grad/ReshapeReshape)gradients/concat_1_grad/Slice_11:output:0(gradients/Reshape_11_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_12_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_12_grad/ReshapeReshape)gradients/concat_1_grad/Slice_12:output:0(gradients/Reshape_12_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_13_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_13_grad/ReshapeReshape)gradients/concat_1_grad/Slice_13:output:0(gradients/Reshape_13_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_14_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_14_grad/ReshapeReshape)gradients/concat_1_grad/Slice_14:output:0(gradients/Reshape_14_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_15_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_15_grad/ReshapeReshape)gradients/concat_1_grad/Slice_15:output:0(gradients/Reshape_15_grad/Shape:output:0*
T0*
_output_shapes
:ю
,gradients/transpose_1_grad/InvertPermutationInvertPermutation=gradients_transpose_1_grad_invertpermutation_transpose_1_perm*
_output_shapes
:х
$gradients/transpose_1_grad/transpose	Transpose'gradients/Reshape_grad/Reshape:output:00gradients/transpose_1_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_2_grad/InvertPermutationInvertPermutation=gradients_transpose_2_grad_invertpermutation_transpose_2_perm*
_output_shapes
:и
$gradients/transpose_2_grad/transpose	Transpose)gradients/Reshape_1_grad/Reshape:output:00gradients/transpose_2_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_3_grad/InvertPermutationInvertPermutation=gradients_transpose_3_grad_invertpermutation_transpose_3_perm*
_output_shapes
:и
$gradients/transpose_3_grad/transpose	Transpose)gradients/Reshape_2_grad/Reshape:output:00gradients/transpose_3_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_4_grad/InvertPermutationInvertPermutation=gradients_transpose_4_grad_invertpermutation_transpose_4_perm*
_output_shapes
:и
$gradients/transpose_4_grad/transpose	Transpose)gradients/Reshape_3_grad/Reshape:output:00gradients/transpose_4_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_5_grad/InvertPermutationInvertPermutation=gradients_transpose_5_grad_invertpermutation_transpose_5_perm*
_output_shapes
:и
$gradients/transpose_5_grad/transpose	Transpose)gradients/Reshape_4_grad/Reshape:output:00gradients/transpose_5_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_6_grad/InvertPermutationInvertPermutation=gradients_transpose_6_grad_invertpermutation_transpose_6_perm*
_output_shapes
:и
$gradients/transpose_6_grad/transpose	Transpose)gradients/Reshape_5_grad/Reshape:output:00gradients/transpose_6_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_7_grad/InvertPermutationInvertPermutation=gradients_transpose_7_grad_invertpermutation_transpose_7_perm*
_output_shapes
:и
$gradients/transpose_7_grad/transpose	Transpose)gradients/Reshape_6_grad/Reshape:output:00gradients/transpose_7_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_8_grad/InvertPermutationInvertPermutation=gradients_transpose_8_grad_invertpermutation_transpose_8_perm*
_output_shapes
:и
$gradients/transpose_8_grad/transpose	Transpose)gradients/Reshape_7_grad/Reshape:output:00gradients/transpose_8_grad/InvertPermutation:y:0*
T0*
_output_shapes

:Т
gradients/split_2_grad/concatConcatV2)gradients/Reshape_8_grad/Reshape:output:0)gradients/Reshape_9_grad/Reshape:output:0*gradients/Reshape_10_grad/Reshape:output:0*gradients/Reshape_11_grad/Reshape:output:0*gradients/Reshape_12_grad/Reshape:output:0*gradients/Reshape_13_grad/Reshape:output:0*gradients/Reshape_14_grad/Reshape:output:0*gradients/Reshape_15_grad/Reshape:output:0/gradients_split_2_grad_concat_split_2_split_dim*
N*
T0*
_output_shapes
:x«
gradients/split_grad/concatConcatV2(gradients/transpose_1_grad/transpose:y:0(gradients/transpose_2_grad/transpose:y:0(gradients/transpose_3_grad/transpose:y:0(gradients/transpose_4_grad/transpose:y:0+gradients_split_grad_concat_split_split_dim*
N*
T0*
_output_shapes

:<┤
gradients/split_1_grad/concatConcatV2(gradients/transpose_5_grad/transpose:y:0(gradients/transpose_6_grad/transpose:y:0(gradients/transpose_7_grad/transpose:y:0(gradients/transpose_8_grad/transpose:y:0/gradients_split_1_grad_concat_split_1_split_dim*
N*
T0*
_output_shapes

:<\
gradients/concat_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :њ
gradients/concat_grad/modFloorMod%gradients_concat_grad_mod_concat_axis#gradients/concat_grad/Rank:output:0*
T0*
_output_shapes
: e
gradients/concat_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:<g
gradients/concat_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:<╩
"gradients/concat_grad/ConcatOffsetConcatOffsetgradients/concat_grad/mod:z:0$gradients/concat_grad/Shape:output:0&gradients/concat_grad/Shape_1:output:0*
N* 
_output_shapes
::Л
gradients/concat_grad/SliceSlice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:0$gradients/concat_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:<Н
gradients/concat_grad/Slice_1Slice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:1&gradients/concat_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:<{
IdentityIdentity&gradients/transpose_grad/transpose:y:0*
T0*4
_output_shapes"
 :                  t

Identity_1Identity*gradients/ExpandDims_grad/Reshape:output:0*
T0*'
_output_shapes
:         v

Identity_2Identity,gradients/ExpandDims_1_grad/Reshape:output:0*
T0*'
_output_shapes
:         e

Identity_3Identity$gradients/split_grad/concat:output:0*
T0*
_output_shapes

:<g

Identity_4Identity&gradients/split_1_grad/concat:output:0*
T0*
_output_shapes

:<c

Identity_5Identity&gradients/concat_grad/Slice_1:output:0*
T0*
_output_shapes
:<"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"!

identity_5Identity_5:output:0*(
_construction_contextkEagerRuntime*Ѓ
_input_shapesы
Ь:         :                  :         :         : :                  ::         :         ::                  :         :         :З::         :         : ::::::::: : : : *=
api_implements+)lstm_8d4f5c85-cfbf-442f-90ec-8d72d94e1b41*
api_preferred_deviceGPU*A
forward_function_name(&__forward_gpu_lstm_with_fallback_74836*
go_backwards( *

time_major( :- )
'
_output_shapes
:         ::6
4
_output_shapes"
 :                  :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: ::6
4
_output_shapes"
 :                  : 

_output_shapes
::1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :	

_output_shapes
:::
6
4
_output_shapes"
 :                  :1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :!

_output_shapes	
:З: 

_output_shapes
::-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: : 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
::

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
Л@
╦
(__inference_gpu_lstm_with_fallback_75134

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4ѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          v
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :                  P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : o

ExpandDims
ExpandDimsinit_hExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : s
ExpandDims_1
ExpandDimsinit_cExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : Ъ
concat_1ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0*
_output_shapes	
:ЗМ
CudnnRNNCudnnRNNtranspose:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1:output:0*
T0*f
_output_shapesT
R:                  :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          Ё
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*4
_output_shapes"
 :                  p
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         f

Identity_1Identitytranspose_9:y:0*
T0*4
_output_shapes"
 :                  Z

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*s
_input_shapesb
`:                  :         :         :<:<:<*=
api_implements+)lstm_2ce075de-0b33-47d3-b5ef-91fa50ccaa52*
api_preferred_deviceGPU*
go_backwards( *

time_major( :\ X
4
_output_shapes"
 :                  
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
і
И
A__inference_lstm_3_layer_call_and_return_conditional_losses_78713

inputs.
read_readvariableop_resource:<0
read_1_readvariableop_resource:<,
read_2_readvariableop_resource:<

identity_3ѕбRead/ReadVariableOpбRead_1/ReadVariableOpбRead_2/ReadVariableOp;
ShapeShapeinputs*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Л
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskP
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :s
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:P
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    l
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:         R
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :w
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:R
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    r
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:         E
ones_like/ShapeShapeinputs*
T0*
_output_shapes
:T
ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ђ?|
	ones_likeFillones_like/Shape:output:0ones_like/Const:output:0*
T0*,
_output_shapes
:         Я]
mulMulinputsones_like:output:0*
T0*,
_output_shapes
:         Яp
Read/ReadVariableOpReadVariableOpread_readvariableop_resource*
_output_shapes

:<*
dtype0Z
IdentityIdentityRead/ReadVariableOp:value:0*
T0*
_output_shapes

:<t
Read_1/ReadVariableOpReadVariableOpread_1_readvariableop_resource*
_output_shapes

:<*
dtype0^

Identity_1IdentityRead_1/ReadVariableOp:value:0*
T0*
_output_shapes

:<p
Read_2/ReadVariableOpReadVariableOpread_2_readvariableop_resource*
_output_shapes
:<*
dtype0Z

Identity_2IdentityRead_2/ReadVariableOp:value:0*
T0*
_output_shapes
:<Х
PartitionedCallPartitionedCallmul:z:0zeros:output:0zeros_1:output:0Identity:output:0Identity_1:output:0Identity_2:output:0*
Tin

2*
Tout	
2*
_collective_manager_ids
 *g
_output_shapesU
S:         :         Я:         :         : * 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8ѓ *(
f#R!
__inference_standard_lstm_78440i

Identity_3IdentityPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         ї
NoOpNoOp^Read/ReadVariableOp^Read_1/ReadVariableOp^Read_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*1
_input_shapes 
:         Я: : : 2*
Read/ReadVariableOpRead/ReadVariableOp2.
Read_1/ReadVariableOpRead_1/ReadVariableOp2.
Read_2/ReadVariableOpRead_2/ReadVariableOp:T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs
Ж┴
ы
9__inference___backward_gpu_lstm_with_fallback_76674_76850
placeholder
placeholder_1
placeholder_2
placeholder_3
placeholder_4/
+gradients_strided_slice_grad_shape_cudnnrnnA
=gradients_transpose_9_grad_invertpermutation_transpose_9_perm)
%gradients_squeeze_grad_shape_cudnnrnn+
'gradients_squeeze_1_grad_shape_cudnnrnn!
gradients_zeros_like_cudnnrnn6
2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose7
3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims9
5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_15
1gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1=
9gradients_transpose_grad_invertpermutation_transpose_perm*
&gradients_expanddims_grad_shape_init_h,
(gradients_expanddims_1_grad_shape_init_c-
)gradients_concat_1_grad_mod_concat_1_axisA
=gradients_transpose_1_grad_invertpermutation_transpose_1_permA
=gradients_transpose_2_grad_invertpermutation_transpose_2_permA
=gradients_transpose_3_grad_invertpermutation_transpose_3_permA
=gradients_transpose_4_grad_invertpermutation_transpose_4_permA
=gradients_transpose_5_grad_invertpermutation_transpose_5_permA
=gradients_transpose_6_grad_invertpermutation_transpose_6_permA
=gradients_transpose_7_grad_invertpermutation_transpose_7_permA
=gradients_transpose_8_grad_invertpermutation_transpose_8_perm3
/gradients_split_2_grad_concat_split_2_split_dim/
+gradients_split_grad_concat_split_split_dim3
/gradients_split_1_grad_concat_split_1_split_dim)
%gradients_concat_grad_mod_concat_axis
identity

identity_1

identity_2

identity_3

identity_4

identity_5ѕ^
gradients/grad_ys_0Identityplaceholder*
T0*'
_output_shapes
:         e
gradients/grad_ys_1Identityplaceholder_1*
T0*,
_output_shapes
:         Я`
gradients/grad_ys_2Identityplaceholder_2*
T0*'
_output_shapes
:         `
gradients/grad_ys_3Identityplaceholder_3*
T0*'
_output_shapes
:         O
gradients/grad_ys_4Identityplaceholder_4*
T0*
_output_shapes
: }
"gradients/strided_slice_grad/ShapeShape+gradients_strided_slice_grad_shape_cudnnrnn*
T0*
_output_shapes
:є
3gradients/strided_slice_grad/StridedSliceGrad/beginConst*
_output_shapes
:*
dtype0*
valueB:
         {
1gradients/strided_slice_grad/StridedSliceGrad/endConst*
_output_shapes
:*
dtype0*
valueB: 
5gradients/strided_slice_grad/StridedSliceGrad/stridesConst*
_output_shapes
:*
dtype0*
valueB:б
-gradients/strided_slice_grad/StridedSliceGradStridedSliceGrad+gradients/strided_slice_grad/Shape:output:0<gradients/strided_slice_grad/StridedSliceGrad/begin:output:0:gradients/strided_slice_grad/StridedSliceGrad/end:output:0>gradients/strided_slice_grad/StridedSliceGrad/strides:output:0gradients/grad_ys_0:output:0*
Index0*
T0*,
_output_shapes
:Я         *
shrink_axis_maskю
,gradients/transpose_9_grad/InvertPermutationInvertPermutation=gradients_transpose_9_grad_invertpermutation_transpose_9_perm*
_output_shapes
:И
$gradients/transpose_9_grad/transpose	Transposegradients/grad_ys_1:output:00gradients/transpose_9_grad/InvertPermutation:y:0*
T0*,
_output_shapes
:Я         q
gradients/Squeeze_grad/ShapeShape%gradients_squeeze_grad_shape_cudnnrnn*
T0*
_output_shapes
:ц
gradients/Squeeze_grad/ReshapeReshapegradients/grad_ys_2:output:0%gradients/Squeeze_grad/Shape:output:0*
T0*+
_output_shapes
:         u
gradients/Squeeze_1_grad/ShapeShape'gradients_squeeze_1_grad_shape_cudnnrnn*
T0*
_output_shapes
:е
 gradients/Squeeze_1_grad/ReshapeReshapegradients/grad_ys_3:output:0'gradients/Squeeze_1_grad/Shape:output:0*
T0*+
_output_shapes
:         Щ
gradients/AddNAddN6gradients/strided_slice_grad/StridedSliceGrad:output:0(gradients/transpose_9_grad/transpose:y:0*
N*
T0*@
_class6
42loc:@gradients/strided_slice_grad/StridedSliceGrad*,
_output_shapes
:Я         c
gradients/zeros_like	ZerosLikegradients_zeros_like_cudnnrnn*
T0*
_output_shapes
:Ђ
(gradients/CudnnRNN_grad/CudnnRNNBackpropCudnnRNNBackprop2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_11gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1+gradients_strided_slice_grad_shape_cudnnrnn%gradients_squeeze_grad_shape_cudnnrnn'gradients_squeeze_1_grad_shape_cudnnrnngradients/AddN:sum:0'gradients/Squeeze_grad/Reshape:output:0)gradients/Squeeze_1_grad/Reshape:output:0gradients_zeros_like_cudnnrnn*
T0*a
_output_shapesO
M:Я         :         :         :Зќ
*gradients/transpose_grad/InvertPermutationInvertPermutation9gradients_transpose_grad_invertpermutation_transpose_perm*
_output_shapes
:Л
"gradients/transpose_grad/transpose	Transpose9gradients/CudnnRNN_grad/CudnnRNNBackprop:input_backprop:0.gradients/transpose_grad/InvertPermutation:y:0*
T0*,
_output_shapes
:         Яu
gradients/ExpandDims_grad/ShapeShape&gradients_expanddims_grad_shape_init_h*
T0*
_output_shapes
:┼
!gradients/ExpandDims_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_h_backprop:0(gradients/ExpandDims_grad/Shape:output:0*
T0*'
_output_shapes
:         y
!gradients/ExpandDims_1_grad/ShapeShape(gradients_expanddims_1_grad_shape_init_c*
T0*
_output_shapes
:╔
#gradients/ExpandDims_1_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_c_backprop:0*gradients/ExpandDims_1_grad/Shape:output:0*
T0*'
_output_shapes
:         ^
gradients/concat_1_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :џ
gradients/concat_1_grad/modFloorMod)gradients_concat_1_grad_mod_concat_1_axis%gradients/concat_1_grad/Rank:output:0*
T0*
_output_shapes
: g
gradients/concat_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_2Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_3Const*
_output_shapes
:*
dtype0*
valueB:j
gradients/concat_1_grad/Shape_4Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_5Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_6Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_7Const*
_output_shapes
:*
dtype0*
valueB:рi
gradients/concat_1_grad/Shape_8Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_9Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_10Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_11Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_12Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_13Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_14Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_15Const*
_output_shapes
:*
dtype0*
valueB:Э
$gradients/concat_1_grad/ConcatOffsetConcatOffsetgradients/concat_1_grad/mod:z:0&gradients/concat_1_grad/Shape:output:0(gradients/concat_1_grad/Shape_1:output:0(gradients/concat_1_grad/Shape_2:output:0(gradients/concat_1_grad/Shape_3:output:0(gradients/concat_1_grad/Shape_4:output:0(gradients/concat_1_grad/Shape_5:output:0(gradients/concat_1_grad/Shape_6:output:0(gradients/concat_1_grad/Shape_7:output:0(gradients/concat_1_grad/Shape_8:output:0(gradients/concat_1_grad/Shape_9:output:0)gradients/concat_1_grad/Shape_10:output:0)gradients/concat_1_grad/Shape_11:output:0)gradients/concat_1_grad/Shape_12:output:0)gradients/concat_1_grad/Shape_13:output:0)gradients/concat_1_grad/Shape_14:output:0)gradients/concat_1_grad/Shape_15:output:0*
N*t
_output_shapesb
`::::::::::::::::в
gradients/concat_1_grad/SliceSlice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:0&gradients/concat_1_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_1Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:1(gradients/concat_1_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_2Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:2(gradients/concat_1_grad/Shape_2:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_3Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:3(gradients/concat_1_grad/Shape_3:output:0*
Index0*
T0*
_output_shapes
:­
gradients/concat_1_grad/Slice_4Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:4(gradients/concat_1_grad/Shape_4:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_5Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:5(gradients/concat_1_grad/Shape_5:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_6Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:6(gradients/concat_1_grad/Shape_6:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_7Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:7(gradients/concat_1_grad/Shape_7:output:0*
Index0*
T0*
_output_shapes	
:р№
gradients/concat_1_grad/Slice_8Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:8(gradients/concat_1_grad/Shape_8:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_9Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:9(gradients/concat_1_grad/Shape_9:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_10Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:10)gradients/concat_1_grad/Shape_10:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_11Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:11)gradients/concat_1_grad/Shape_11:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_12Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:12)gradients/concat_1_grad/Shape_12:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_13Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:13)gradients/concat_1_grad/Shape_13:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_14Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:14)gradients/concat_1_grad/Shape_14:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_15Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:15)gradients/concat_1_grad/Shape_15:output:0*
Index0*
T0*
_output_shapes
:m
gradients/Reshape_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      А
gradients/Reshape_grad/ReshapeReshape&gradients/concat_1_grad/Slice:output:0%gradients/Reshape_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_1_grad/ReshapeReshape(gradients/concat_1_grad/Slice_1:output:0'gradients/Reshape_1_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_2_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_2_grad/ReshapeReshape(gradients/concat_1_grad/Slice_2:output:0'gradients/Reshape_2_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_3_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_3_grad/ReshapeReshape(gradients/concat_1_grad/Slice_3:output:0'gradients/Reshape_3_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_4_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_4_grad/ReshapeReshape(gradients/concat_1_grad/Slice_4:output:0'gradients/Reshape_4_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_5_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_5_grad/ReshapeReshape(gradients/concat_1_grad/Slice_5:output:0'gradients/Reshape_5_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_6_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_6_grad/ReshapeReshape(gradients/concat_1_grad/Slice_6:output:0'gradients/Reshape_6_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_7_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_7_grad/ReshapeReshape(gradients/concat_1_grad/Slice_7:output:0'gradients/Reshape_7_grad/Shape:output:0*
T0*
_output_shapes

:h
gradients/Reshape_8_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_8_grad/ReshapeReshape(gradients/concat_1_grad/Slice_8:output:0'gradients/Reshape_8_grad/Shape:output:0*
T0*
_output_shapes
:h
gradients/Reshape_9_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_9_grad/ReshapeReshape(gradients/concat_1_grad/Slice_9:output:0'gradients/Reshape_9_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_10_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_10_grad/ReshapeReshape)gradients/concat_1_grad/Slice_10:output:0(gradients/Reshape_10_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_11_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_11_grad/ReshapeReshape)gradients/concat_1_grad/Slice_11:output:0(gradients/Reshape_11_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_12_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_12_grad/ReshapeReshape)gradients/concat_1_grad/Slice_12:output:0(gradients/Reshape_12_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_13_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_13_grad/ReshapeReshape)gradients/concat_1_grad/Slice_13:output:0(gradients/Reshape_13_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_14_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_14_grad/ReshapeReshape)gradients/concat_1_grad/Slice_14:output:0(gradients/Reshape_14_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_15_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_15_grad/ReshapeReshape)gradients/concat_1_grad/Slice_15:output:0(gradients/Reshape_15_grad/Shape:output:0*
T0*
_output_shapes
:ю
,gradients/transpose_1_grad/InvertPermutationInvertPermutation=gradients_transpose_1_grad_invertpermutation_transpose_1_perm*
_output_shapes
:х
$gradients/transpose_1_grad/transpose	Transpose'gradients/Reshape_grad/Reshape:output:00gradients/transpose_1_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_2_grad/InvertPermutationInvertPermutation=gradients_transpose_2_grad_invertpermutation_transpose_2_perm*
_output_shapes
:и
$gradients/transpose_2_grad/transpose	Transpose)gradients/Reshape_1_grad/Reshape:output:00gradients/transpose_2_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_3_grad/InvertPermutationInvertPermutation=gradients_transpose_3_grad_invertpermutation_transpose_3_perm*
_output_shapes
:и
$gradients/transpose_3_grad/transpose	Transpose)gradients/Reshape_2_grad/Reshape:output:00gradients/transpose_3_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_4_grad/InvertPermutationInvertPermutation=gradients_transpose_4_grad_invertpermutation_transpose_4_perm*
_output_shapes
:и
$gradients/transpose_4_grad/transpose	Transpose)gradients/Reshape_3_grad/Reshape:output:00gradients/transpose_4_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_5_grad/InvertPermutationInvertPermutation=gradients_transpose_5_grad_invertpermutation_transpose_5_perm*
_output_shapes
:и
$gradients/transpose_5_grad/transpose	Transpose)gradients/Reshape_4_grad/Reshape:output:00gradients/transpose_5_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_6_grad/InvertPermutationInvertPermutation=gradients_transpose_6_grad_invertpermutation_transpose_6_perm*
_output_shapes
:и
$gradients/transpose_6_grad/transpose	Transpose)gradients/Reshape_5_grad/Reshape:output:00gradients/transpose_6_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_7_grad/InvertPermutationInvertPermutation=gradients_transpose_7_grad_invertpermutation_transpose_7_perm*
_output_shapes
:и
$gradients/transpose_7_grad/transpose	Transpose)gradients/Reshape_6_grad/Reshape:output:00gradients/transpose_7_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_8_grad/InvertPermutationInvertPermutation=gradients_transpose_8_grad_invertpermutation_transpose_8_perm*
_output_shapes
:и
$gradients/transpose_8_grad/transpose	Transpose)gradients/Reshape_7_grad/Reshape:output:00gradients/transpose_8_grad/InvertPermutation:y:0*
T0*
_output_shapes

:Т
gradients/split_2_grad/concatConcatV2)gradients/Reshape_8_grad/Reshape:output:0)gradients/Reshape_9_grad/Reshape:output:0*gradients/Reshape_10_grad/Reshape:output:0*gradients/Reshape_11_grad/Reshape:output:0*gradients/Reshape_12_grad/Reshape:output:0*gradients/Reshape_13_grad/Reshape:output:0*gradients/Reshape_14_grad/Reshape:output:0*gradients/Reshape_15_grad/Reshape:output:0/gradients_split_2_grad_concat_split_2_split_dim*
N*
T0*
_output_shapes
:x«
gradients/split_grad/concatConcatV2(gradients/transpose_1_grad/transpose:y:0(gradients/transpose_2_grad/transpose:y:0(gradients/transpose_3_grad/transpose:y:0(gradients/transpose_4_grad/transpose:y:0+gradients_split_grad_concat_split_split_dim*
N*
T0*
_output_shapes

:<┤
gradients/split_1_grad/concatConcatV2(gradients/transpose_5_grad/transpose:y:0(gradients/transpose_6_grad/transpose:y:0(gradients/transpose_7_grad/transpose:y:0(gradients/transpose_8_grad/transpose:y:0/gradients_split_1_grad_concat_split_1_split_dim*
N*
T0*
_output_shapes

:<\
gradients/concat_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :њ
gradients/concat_grad/modFloorMod%gradients_concat_grad_mod_concat_axis#gradients/concat_grad/Rank:output:0*
T0*
_output_shapes
: e
gradients/concat_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:<g
gradients/concat_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:<╩
"gradients/concat_grad/ConcatOffsetConcatOffsetgradients/concat_grad/mod:z:0$gradients/concat_grad/Shape:output:0&gradients/concat_grad/Shape_1:output:0*
N* 
_output_shapes
::Л
gradients/concat_grad/SliceSlice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:0$gradients/concat_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:<Н
gradients/concat_grad/Slice_1Slice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:1&gradients/concat_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:<s
IdentityIdentity&gradients/transpose_grad/transpose:y:0*
T0*,
_output_shapes
:         Яt

Identity_1Identity*gradients/ExpandDims_grad/Reshape:output:0*
T0*'
_output_shapes
:         v

Identity_2Identity,gradients/ExpandDims_1_grad/Reshape:output:0*
T0*'
_output_shapes
:         e

Identity_3Identity$gradients/split_grad/concat:output:0*
T0*
_output_shapes

:<g

Identity_4Identity&gradients/split_1_grad/concat:output:0*
T0*
_output_shapes

:<c

Identity_5Identity&gradients/concat_grad/Slice_1:output:0*
T0*
_output_shapes
:<"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"!

identity_5Identity_5:output:0*(
_construction_contextkEagerRuntime*в
_input_shapes┘
о:         :         Я:         :         : :Я         ::         :         ::Я         :         :         :З::         :         : ::::::::: : : : *=
api_implements+)lstm_a92946bc-3bab-4632-a518-695c88a7828f*
api_preferred_deviceGPU*A
forward_function_name(&__forward_gpu_lstm_with_fallback_76849*
go_backwards( *

time_major( :- )
'
_output_shapes
:         :2.
,
_output_shapes
:         Я:-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :2.
,
_output_shapes
:Я         : 

_output_shapes
::1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :	

_output_shapes
::2
.
,
_output_shapes
:Я         :1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :!

_output_shapes	
:З: 

_output_shapes
::-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: : 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
::

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
дL
Б
&__forward_gpu_lstm_with_fallback_77816

inputs
init_h_0
init_c_0

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4
cudnnrnn
transpose_9_perm

cudnnrnn_0

cudnnrnn_1

cudnnrnn_2
	transpose

expanddims
expanddims_1
concat_1
transpose_perm

init_h

init_c
concat_1_axis
transpose_1_perm
transpose_2_perm
transpose_3_perm
transpose_4_perm
transpose_5_perm
transpose_6_perm
transpose_7_perm
transpose_8_perm
split_2_split_dim
split_split_dim
split_1_split_dim
concat_axisѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          B
transpose_0	Transposeinputstranspose/perm:output:0*
T0P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : q

ExpandDims
ExpandDimsinit_h_0ExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : u
ExpandDims_1
ExpandDimsinit_c_0ExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : ё

concat_1_0ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0О
CudnnRNNCudnnRNNtranspose_0:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1_0:output:0*
T0*f
_output_shapesT
R:                  :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          Ё
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*4
_output_shapes"
 :                  p
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         f

Identity_1Identitytranspose_9:y:0*
T0*4
_output_shapes"
 :                  Z

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
concat_1concat_1_0:output:0"'
concat_1_axisconcat_1/axis:output:0"#
concat_axisconcat/axis:output:0"
cudnnrnnCudnnRNN:output:0"!

cudnnrnn_0CudnnRNN:output_h:0"!

cudnnrnn_1CudnnRNN:output_c:0"&

cudnnrnn_2CudnnRNN:reserve_space:0"!

expanddimsExpandDims:output:0"%
expanddims_1ExpandDims_1:output:0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"
init_cinit_c_0"
init_hinit_h_0"/
split_1_split_dimsplit_1/split_dim:output:0"/
split_2_split_dimsplit_2/split_dim:output:0"+
split_split_dimsplit/split_dim:output:0"
	transposetranspose_0:y:0"-
transpose_1_permtranspose_1/perm:output:0"-
transpose_2_permtranspose_2/perm:output:0"-
transpose_3_permtranspose_3/perm:output:0"-
transpose_4_permtranspose_4/perm:output:0"-
transpose_5_permtranspose_5/perm:output:0"-
transpose_6_permtranspose_6/perm:output:0"-
transpose_7_permtranspose_7/perm:output:0"-
transpose_8_permtranspose_8/perm:output:0"-
transpose_9_permtranspose_9/perm:output:0")
transpose_permtranspose/perm:output:0*(
_construction_contextkEagerRuntime*s
_input_shapesb
`:                  :         :         :<:<:<*=
api_implements+)lstm_9e12c0bd-db3f-4f45-bd70-4aa03a8d3462*
api_preferred_deviceGPU*U
backward_function_name;9__inference___backward_gpu_lstm_with_fallback_77641_77817*
go_backwards( *

time_major( :\ X
4
_output_shapes"
 :                  
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
§K
Б
&__forward_gpu_lstm_with_fallback_79173

inputs
init_h_0
init_c_0

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4
cudnnrnn
transpose_9_perm

cudnnrnn_0

cudnnrnn_1

cudnnrnn_2
	transpose

expanddims
expanddims_1
concat_1
transpose_perm

init_h

init_c
concat_1_axis
transpose_1_perm
transpose_2_perm
transpose_3_perm
transpose_4_perm
transpose_5_perm
transpose_6_perm
transpose_7_perm
transpose_8_perm
split_2_split_dim
split_split_dim
split_1_split_dim
concat_axisѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          B
transpose_0	Transposeinputstranspose/perm:output:0*
T0P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : q

ExpandDims
ExpandDimsinit_h_0ExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : u
ExpandDims_1
ExpandDimsinit_c_0ExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : ё

concat_1_0ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0¤
CudnnRNNCudnnRNNtranspose_0:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1_0:output:0*
T0*^
_output_shapesL
J:Я         :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          }
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*,
_output_shapes
:         Яp
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_9:y:0*
T0*,
_output_shapes
:         ЯZ

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
concat_1concat_1_0:output:0"'
concat_1_axisconcat_1/axis:output:0"#
concat_axisconcat/axis:output:0"
cudnnrnnCudnnRNN:output:0"!

cudnnrnn_0CudnnRNN:output_h:0"!

cudnnrnn_1CudnnRNN:output_c:0"&

cudnnrnn_2CudnnRNN:reserve_space:0"!

expanddimsExpandDims:output:0"%
expanddims_1ExpandDims_1:output:0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"
init_cinit_c_0"
init_hinit_h_0"/
split_1_split_dimsplit_1/split_dim:output:0"/
split_2_split_dimsplit_2/split_dim:output:0"+
split_split_dimsplit/split_dim:output:0"
	transposetranspose_0:y:0"-
transpose_1_permtranspose_1/perm:output:0"-
transpose_2_permtranspose_2/perm:output:0"-
transpose_3_permtranspose_3/perm:output:0"-
transpose_4_permtranspose_4/perm:output:0"-
transpose_5_permtranspose_5/perm:output:0"-
transpose_6_permtranspose_6/perm:output:0"-
transpose_7_permtranspose_7/perm:output:0"-
transpose_8_permtranspose_8/perm:output:0"-
transpose_9_permtranspose_9/perm:output:0")
transpose_permtranspose/perm:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_428e9d17-ff82-4115-bdf7-a5882938e408*
api_preferred_deviceGPU*U
backward_function_name;9__inference___backward_gpu_lstm_with_fallback_78998_79174*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
а@
╦
(__inference_gpu_lstm_with_fallback_77142

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4ѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          n
	transpose	Transposeinputstranspose/perm:output:0*
T0*,
_output_shapes
:Я         P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : o

ExpandDims
ExpandDimsinit_hExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : s
ExpandDims_1
ExpandDimsinit_cExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : Ъ
concat_1ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0*
_output_shapes	
:З╦
CudnnRNNCudnnRNNtranspose:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1:output:0*
T0*^
_output_shapesL
J:Я         :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          }
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*,
_output_shapes
:         Яp
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_9:y:0*
T0*,
_output_shapes
:         ЯZ

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_91ae4c3a-1bb4-4840-80b7-ab1926c68132*
api_preferred_deviceGPU*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
й
║
A__inference_lstm_3_layer_call_and_return_conditional_losses_77819
inputs_0.
read_readvariableop_resource:<0
read_1_readvariableop_resource:<,
read_2_readvariableop_resource:<

identity_3ѕбRead/ReadVariableOpбRead_1/ReadVariableOpбRead_2/ReadVariableOp=
ShapeShapeinputs_0*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Л
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskP
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :s
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:P
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    l
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:         R
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :w
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:R
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    r
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:         G
ones_like/ShapeShapeinputs_0*
T0*
_output_shapes
:T
ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ђ?ё
	ones_likeFillones_like/Shape:output:0ones_like/Const:output:0*
T0*4
_output_shapes"
 :                  g
mulMulinputs_0ones_like:output:0*
T0*4
_output_shapes"
 :                  p
Read/ReadVariableOpReadVariableOpread_readvariableop_resource*
_output_shapes

:<*
dtype0Z
IdentityIdentityRead/ReadVariableOp:value:0*
T0*
_output_shapes

:<t
Read_1/ReadVariableOpReadVariableOpread_1_readvariableop_resource*
_output_shapes

:<*
dtype0^

Identity_1IdentityRead_1/ReadVariableOp:value:0*
T0*
_output_shapes

:<p
Read_2/ReadVariableOpReadVariableOpread_2_readvariableop_resource*
_output_shapes
:<*
dtype0Z

Identity_2IdentityRead_2/ReadVariableOp:value:0*
T0*
_output_shapes
:<Й
PartitionedCallPartitionedCallmul:z:0zeros:output:0zeros_1:output:0Identity:output:0Identity_1:output:0Identity_2:output:0*
Tin

2*
Tout	
2*
_collective_manager_ids
 *o
_output_shapes]
[:         :                  :         :         : * 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8ѓ *(
f#R!
__inference_standard_lstm_77546i

Identity_3IdentityPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         ї
NoOpNoOp^Read/ReadVariableOp^Read_1/ReadVariableOp^Read_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:                  : : : 2*
Read/ReadVariableOpRead/ReadVariableOp2.
Read_1/ReadVariableOpRead_1/ReadVariableOp2.
Read_2/ReadVariableOpRead_2/ReadVariableOp:^ Z
4
_output_shapes"
 :                  
"
_user_specified_name
inputs/0
дL
Б
&__forward_gpu_lstm_with_fallback_78279

inputs
init_h_0
init_c_0

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4
cudnnrnn
transpose_9_perm

cudnnrnn_0

cudnnrnn_1

cudnnrnn_2
	transpose

expanddims
expanddims_1
concat_1
transpose_perm

init_h

init_c
concat_1_axis
transpose_1_perm
transpose_2_perm
transpose_3_perm
transpose_4_perm
transpose_5_perm
transpose_6_perm
transpose_7_perm
transpose_8_perm
split_2_split_dim
split_split_dim
split_1_split_dim
concat_axisѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          B
transpose_0	Transposeinputstranspose/perm:output:0*
T0P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : q

ExpandDims
ExpandDimsinit_h_0ExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : u
ExpandDims_1
ExpandDimsinit_c_0ExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : ё

concat_1_0ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0О
CudnnRNNCudnnRNNtranspose_0:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1_0:output:0*
T0*f
_output_shapesT
R:                  :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          Ё
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*4
_output_shapes"
 :                  p
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         f

Identity_1Identitytranspose_9:y:0*
T0*4
_output_shapes"
 :                  Z

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
concat_1concat_1_0:output:0"'
concat_1_axisconcat_1/axis:output:0"#
concat_axisconcat/axis:output:0"
cudnnrnnCudnnRNN:output:0"!

cudnnrnn_0CudnnRNN:output_h:0"!

cudnnrnn_1CudnnRNN:output_c:0"&

cudnnrnn_2CudnnRNN:reserve_space:0"!

expanddimsExpandDims:output:0"%
expanddims_1ExpandDims_1:output:0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"
init_cinit_c_0"
init_hinit_h_0"/
split_1_split_dimsplit_1/split_dim:output:0"/
split_2_split_dimsplit_2/split_dim:output:0"+
split_split_dimsplit/split_dim:output:0"
	transposetranspose_0:y:0"-
transpose_1_permtranspose_1/perm:output:0"-
transpose_2_permtranspose_2/perm:output:0"-
transpose_3_permtranspose_3/perm:output:0"-
transpose_4_permtranspose_4/perm:output:0"-
transpose_5_permtranspose_5/perm:output:0"-
transpose_6_permtranspose_6/perm:output:0"-
transpose_7_permtranspose_7/perm:output:0"-
transpose_8_permtranspose_8/perm:output:0"-
transpose_9_permtranspose_9/perm:output:0")
transpose_permtranspose/perm:output:0*(
_construction_contextkEagerRuntime*s
_input_shapesb
`:                  :         :         :<:<:<*=
api_implements+)lstm_bb744fac-7292-4ecb-affd-a726f7708b09*
api_preferred_deviceGPU*U
backward_function_name;9__inference___backward_gpu_lstm_with_fallback_78104_78280*
go_backwards( *

time_major( :\ X
4
_output_shapes"
 :                  
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
І
▓
&__inference_lstm_3_layer_call_fn_77355
inputs_0
unknown:<
	unknown_0:<
	unknown_1:<
identityѕбStatefulPartitionedCallУ
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:         *%
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8ѓ *J
fERC
A__inference_lstm_3_layer_call_and_return_conditional_losses_74839o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         `
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:                  : : : 22
StatefulPartitionedCallStatefulPartitionedCall:^ Z
4
_output_shapes"
 :                  
"
_user_specified_name
inputs/0
Ж┴
ы
9__inference___backward_gpu_lstm_with_fallback_74220_74396
placeholder
placeholder_1
placeholder_2
placeholder_3
placeholder_4/
+gradients_strided_slice_grad_shape_cudnnrnnA
=gradients_transpose_9_grad_invertpermutation_transpose_9_perm)
%gradients_squeeze_grad_shape_cudnnrnn+
'gradients_squeeze_1_grad_shape_cudnnrnn!
gradients_zeros_like_cudnnrnn6
2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose7
3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims9
5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_15
1gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1=
9gradients_transpose_grad_invertpermutation_transpose_perm*
&gradients_expanddims_grad_shape_init_h,
(gradients_expanddims_1_grad_shape_init_c-
)gradients_concat_1_grad_mod_concat_1_axisA
=gradients_transpose_1_grad_invertpermutation_transpose_1_permA
=gradients_transpose_2_grad_invertpermutation_transpose_2_permA
=gradients_transpose_3_grad_invertpermutation_transpose_3_permA
=gradients_transpose_4_grad_invertpermutation_transpose_4_permA
=gradients_transpose_5_grad_invertpermutation_transpose_5_permA
=gradients_transpose_6_grad_invertpermutation_transpose_6_permA
=gradients_transpose_7_grad_invertpermutation_transpose_7_permA
=gradients_transpose_8_grad_invertpermutation_transpose_8_perm3
/gradients_split_2_grad_concat_split_2_split_dim/
+gradients_split_grad_concat_split_split_dim3
/gradients_split_1_grad_concat_split_1_split_dim)
%gradients_concat_grad_mod_concat_axis
identity

identity_1

identity_2

identity_3

identity_4

identity_5ѕ^
gradients/grad_ys_0Identityplaceholder*
T0*'
_output_shapes
:         e
gradients/grad_ys_1Identityplaceholder_1*
T0*,
_output_shapes
:         Я`
gradients/grad_ys_2Identityplaceholder_2*
T0*'
_output_shapes
:         `
gradients/grad_ys_3Identityplaceholder_3*
T0*'
_output_shapes
:         O
gradients/grad_ys_4Identityplaceholder_4*
T0*
_output_shapes
: }
"gradients/strided_slice_grad/ShapeShape+gradients_strided_slice_grad_shape_cudnnrnn*
T0*
_output_shapes
:є
3gradients/strided_slice_grad/StridedSliceGrad/beginConst*
_output_shapes
:*
dtype0*
valueB:
         {
1gradients/strided_slice_grad/StridedSliceGrad/endConst*
_output_shapes
:*
dtype0*
valueB: 
5gradients/strided_slice_grad/StridedSliceGrad/stridesConst*
_output_shapes
:*
dtype0*
valueB:б
-gradients/strided_slice_grad/StridedSliceGradStridedSliceGrad+gradients/strided_slice_grad/Shape:output:0<gradients/strided_slice_grad/StridedSliceGrad/begin:output:0:gradients/strided_slice_grad/StridedSliceGrad/end:output:0>gradients/strided_slice_grad/StridedSliceGrad/strides:output:0gradients/grad_ys_0:output:0*
Index0*
T0*,
_output_shapes
:Я         *
shrink_axis_maskю
,gradients/transpose_9_grad/InvertPermutationInvertPermutation=gradients_transpose_9_grad_invertpermutation_transpose_9_perm*
_output_shapes
:И
$gradients/transpose_9_grad/transpose	Transposegradients/grad_ys_1:output:00gradients/transpose_9_grad/InvertPermutation:y:0*
T0*,
_output_shapes
:Я         q
gradients/Squeeze_grad/ShapeShape%gradients_squeeze_grad_shape_cudnnrnn*
T0*
_output_shapes
:ц
gradients/Squeeze_grad/ReshapeReshapegradients/grad_ys_2:output:0%gradients/Squeeze_grad/Shape:output:0*
T0*+
_output_shapes
:         u
gradients/Squeeze_1_grad/ShapeShape'gradients_squeeze_1_grad_shape_cudnnrnn*
T0*
_output_shapes
:е
 gradients/Squeeze_1_grad/ReshapeReshapegradients/grad_ys_3:output:0'gradients/Squeeze_1_grad/Shape:output:0*
T0*+
_output_shapes
:         Щ
gradients/AddNAddN6gradients/strided_slice_grad/StridedSliceGrad:output:0(gradients/transpose_9_grad/transpose:y:0*
N*
T0*@
_class6
42loc:@gradients/strided_slice_grad/StridedSliceGrad*,
_output_shapes
:Я         c
gradients/zeros_like	ZerosLikegradients_zeros_like_cudnnrnn*
T0*
_output_shapes
:Ђ
(gradients/CudnnRNN_grad/CudnnRNNBackpropCudnnRNNBackprop2gradients_cudnnrnn_grad_cudnnrnnbackprop_transpose3gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims5gradients_cudnnrnn_grad_cudnnrnnbackprop_expanddims_11gradients_cudnnrnn_grad_cudnnrnnbackprop_concat_1+gradients_strided_slice_grad_shape_cudnnrnn%gradients_squeeze_grad_shape_cudnnrnn'gradients_squeeze_1_grad_shape_cudnnrnngradients/AddN:sum:0'gradients/Squeeze_grad/Reshape:output:0)gradients/Squeeze_1_grad/Reshape:output:0gradients_zeros_like_cudnnrnn*
T0*a
_output_shapesO
M:Я         :         :         :Зќ
*gradients/transpose_grad/InvertPermutationInvertPermutation9gradients_transpose_grad_invertpermutation_transpose_perm*
_output_shapes
:Л
"gradients/transpose_grad/transpose	Transpose9gradients/CudnnRNN_grad/CudnnRNNBackprop:input_backprop:0.gradients/transpose_grad/InvertPermutation:y:0*
T0*,
_output_shapes
:         Яu
gradients/ExpandDims_grad/ShapeShape&gradients_expanddims_grad_shape_init_h*
T0*
_output_shapes
:┼
!gradients/ExpandDims_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_h_backprop:0(gradients/ExpandDims_grad/Shape:output:0*
T0*'
_output_shapes
:         y
!gradients/ExpandDims_1_grad/ShapeShape(gradients_expanddims_1_grad_shape_init_c*
T0*
_output_shapes
:╔
#gradients/ExpandDims_1_grad/ReshapeReshape;gradients/CudnnRNN_grad/CudnnRNNBackprop:input_c_backprop:0*gradients/ExpandDims_1_grad/Shape:output:0*
T0*'
_output_shapes
:         ^
gradients/concat_1_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :џ
gradients/concat_1_grad/modFloorMod)gradients_concat_1_grad_mod_concat_1_axis%gradients/concat_1_grad/Rank:output:0*
T0*
_output_shapes
: g
gradients/concat_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_2Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_3Const*
_output_shapes
:*
dtype0*
valueB:j
gradients/concat_1_grad/Shape_4Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_5Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_6Const*
_output_shapes
:*
dtype0*
valueB:рj
gradients/concat_1_grad/Shape_7Const*
_output_shapes
:*
dtype0*
valueB:рi
gradients/concat_1_grad/Shape_8Const*
_output_shapes
:*
dtype0*
valueB:i
gradients/concat_1_grad/Shape_9Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_10Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_11Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_12Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_13Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_14Const*
_output_shapes
:*
dtype0*
valueB:j
 gradients/concat_1_grad/Shape_15Const*
_output_shapes
:*
dtype0*
valueB:Э
$gradients/concat_1_grad/ConcatOffsetConcatOffsetgradients/concat_1_grad/mod:z:0&gradients/concat_1_grad/Shape:output:0(gradients/concat_1_grad/Shape_1:output:0(gradients/concat_1_grad/Shape_2:output:0(gradients/concat_1_grad/Shape_3:output:0(gradients/concat_1_grad/Shape_4:output:0(gradients/concat_1_grad/Shape_5:output:0(gradients/concat_1_grad/Shape_6:output:0(gradients/concat_1_grad/Shape_7:output:0(gradients/concat_1_grad/Shape_8:output:0(gradients/concat_1_grad/Shape_9:output:0)gradients/concat_1_grad/Shape_10:output:0)gradients/concat_1_grad/Shape_11:output:0)gradients/concat_1_grad/Shape_12:output:0)gradients/concat_1_grad/Shape_13:output:0)gradients/concat_1_grad/Shape_14:output:0)gradients/concat_1_grad/Shape_15:output:0*
N*t
_output_shapesb
`::::::::::::::::в
gradients/concat_1_grad/SliceSlice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:0&gradients/concat_1_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_1Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:1(gradients/concat_1_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_2Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:2(gradients/concat_1_grad/Shape_2:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_3Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:3(gradients/concat_1_grad/Shape_3:output:0*
Index0*
T0*
_output_shapes
:­
gradients/concat_1_grad/Slice_4Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:4(gradients/concat_1_grad/Shape_4:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_5Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:5(gradients/concat_1_grad/Shape_5:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_6Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:6(gradients/concat_1_grad/Shape_6:output:0*
Index0*
T0*
_output_shapes	
:р­
gradients/concat_1_grad/Slice_7Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:7(gradients/concat_1_grad/Shape_7:output:0*
Index0*
T0*
_output_shapes	
:р№
gradients/concat_1_grad/Slice_8Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:8(gradients/concat_1_grad/Shape_8:output:0*
Index0*
T0*
_output_shapes
:№
gradients/concat_1_grad/Slice_9Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0-gradients/concat_1_grad/ConcatOffset:offset:9(gradients/concat_1_grad/Shape_9:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_10Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:10)gradients/concat_1_grad/Shape_10:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_11Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:11)gradients/concat_1_grad/Shape_11:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_12Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:12)gradients/concat_1_grad/Shape_12:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_13Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:13)gradients/concat_1_grad/Shape_13:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_14Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:14)gradients/concat_1_grad/Shape_14:output:0*
Index0*
T0*
_output_shapes
:Ы
 gradients/concat_1_grad/Slice_15Slice:gradients/CudnnRNN_grad/CudnnRNNBackprop:params_backprop:0.gradients/concat_1_grad/ConcatOffset:offset:15)gradients/concat_1_grad/Shape_15:output:0*
Index0*
T0*
_output_shapes
:m
gradients/Reshape_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      А
gradients/Reshape_grad/ReshapeReshape&gradients/concat_1_grad/Slice:output:0%gradients/Reshape_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_1_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_1_grad/ReshapeReshape(gradients/concat_1_grad/Slice_1:output:0'gradients/Reshape_1_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_2_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_2_grad/ReshapeReshape(gradients/concat_1_grad/Slice_2:output:0'gradients/Reshape_2_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_3_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_3_grad/ReshapeReshape(gradients/concat_1_grad/Slice_3:output:0'gradients/Reshape_3_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_4_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_4_grad/ReshapeReshape(gradients/concat_1_grad/Slice_4:output:0'gradients/Reshape_4_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_5_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_5_grad/ReshapeReshape(gradients/concat_1_grad/Slice_5:output:0'gradients/Reshape_5_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_6_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_6_grad/ReshapeReshape(gradients/concat_1_grad/Slice_6:output:0'gradients/Reshape_6_grad/Shape:output:0*
T0*
_output_shapes

:o
gradients/Reshape_7_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB"      Д
 gradients/Reshape_7_grad/ReshapeReshape(gradients/concat_1_grad/Slice_7:output:0'gradients/Reshape_7_grad/Shape:output:0*
T0*
_output_shapes

:h
gradients/Reshape_8_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_8_grad/ReshapeReshape(gradients/concat_1_grad/Slice_8:output:0'gradients/Reshape_8_grad/Shape:output:0*
T0*
_output_shapes
:h
gradients/Reshape_9_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:Б
 gradients/Reshape_9_grad/ReshapeReshape(gradients/concat_1_grad/Slice_9:output:0'gradients/Reshape_9_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_10_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_10_grad/ReshapeReshape)gradients/concat_1_grad/Slice_10:output:0(gradients/Reshape_10_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_11_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_11_grad/ReshapeReshape)gradients/concat_1_grad/Slice_11:output:0(gradients/Reshape_11_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_12_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_12_grad/ReshapeReshape)gradients/concat_1_grad/Slice_12:output:0(gradients/Reshape_12_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_13_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_13_grad/ReshapeReshape)gradients/concat_1_grad/Slice_13:output:0(gradients/Reshape_13_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_14_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_14_grad/ReshapeReshape)gradients/concat_1_grad/Slice_14:output:0(gradients/Reshape_14_grad/Shape:output:0*
T0*
_output_shapes
:i
gradients/Reshape_15_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:д
!gradients/Reshape_15_grad/ReshapeReshape)gradients/concat_1_grad/Slice_15:output:0(gradients/Reshape_15_grad/Shape:output:0*
T0*
_output_shapes
:ю
,gradients/transpose_1_grad/InvertPermutationInvertPermutation=gradients_transpose_1_grad_invertpermutation_transpose_1_perm*
_output_shapes
:х
$gradients/transpose_1_grad/transpose	Transpose'gradients/Reshape_grad/Reshape:output:00gradients/transpose_1_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_2_grad/InvertPermutationInvertPermutation=gradients_transpose_2_grad_invertpermutation_transpose_2_perm*
_output_shapes
:и
$gradients/transpose_2_grad/transpose	Transpose)gradients/Reshape_1_grad/Reshape:output:00gradients/transpose_2_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_3_grad/InvertPermutationInvertPermutation=gradients_transpose_3_grad_invertpermutation_transpose_3_perm*
_output_shapes
:и
$gradients/transpose_3_grad/transpose	Transpose)gradients/Reshape_2_grad/Reshape:output:00gradients/transpose_3_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_4_grad/InvertPermutationInvertPermutation=gradients_transpose_4_grad_invertpermutation_transpose_4_perm*
_output_shapes
:и
$gradients/transpose_4_grad/transpose	Transpose)gradients/Reshape_3_grad/Reshape:output:00gradients/transpose_4_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_5_grad/InvertPermutationInvertPermutation=gradients_transpose_5_grad_invertpermutation_transpose_5_perm*
_output_shapes
:и
$gradients/transpose_5_grad/transpose	Transpose)gradients/Reshape_4_grad/Reshape:output:00gradients/transpose_5_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_6_grad/InvertPermutationInvertPermutation=gradients_transpose_6_grad_invertpermutation_transpose_6_perm*
_output_shapes
:и
$gradients/transpose_6_grad/transpose	Transpose)gradients/Reshape_5_grad/Reshape:output:00gradients/transpose_6_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_7_grad/InvertPermutationInvertPermutation=gradients_transpose_7_grad_invertpermutation_transpose_7_perm*
_output_shapes
:и
$gradients/transpose_7_grad/transpose	Transpose)gradients/Reshape_6_grad/Reshape:output:00gradients/transpose_7_grad/InvertPermutation:y:0*
T0*
_output_shapes

:ю
,gradients/transpose_8_grad/InvertPermutationInvertPermutation=gradients_transpose_8_grad_invertpermutation_transpose_8_perm*
_output_shapes
:и
$gradients/transpose_8_grad/transpose	Transpose)gradients/Reshape_7_grad/Reshape:output:00gradients/transpose_8_grad/InvertPermutation:y:0*
T0*
_output_shapes

:Т
gradients/split_2_grad/concatConcatV2)gradients/Reshape_8_grad/Reshape:output:0)gradients/Reshape_9_grad/Reshape:output:0*gradients/Reshape_10_grad/Reshape:output:0*gradients/Reshape_11_grad/Reshape:output:0*gradients/Reshape_12_grad/Reshape:output:0*gradients/Reshape_13_grad/Reshape:output:0*gradients/Reshape_14_grad/Reshape:output:0*gradients/Reshape_15_grad/Reshape:output:0/gradients_split_2_grad_concat_split_2_split_dim*
N*
T0*
_output_shapes
:x«
gradients/split_grad/concatConcatV2(gradients/transpose_1_grad/transpose:y:0(gradients/transpose_2_grad/transpose:y:0(gradients/transpose_3_grad/transpose:y:0(gradients/transpose_4_grad/transpose:y:0+gradients_split_grad_concat_split_split_dim*
N*
T0*
_output_shapes

:<┤
gradients/split_1_grad/concatConcatV2(gradients/transpose_5_grad/transpose:y:0(gradients/transpose_6_grad/transpose:y:0(gradients/transpose_7_grad/transpose:y:0(gradients/transpose_8_grad/transpose:y:0/gradients_split_1_grad_concat_split_1_split_dim*
N*
T0*
_output_shapes

:<\
gradients/concat_grad/RankConst*
_output_shapes
: *
dtype0*
value	B :њ
gradients/concat_grad/modFloorMod%gradients_concat_grad_mod_concat_axis#gradients/concat_grad/Rank:output:0*
T0*
_output_shapes
: e
gradients/concat_grad/ShapeConst*
_output_shapes
:*
dtype0*
valueB:<g
gradients/concat_grad/Shape_1Const*
_output_shapes
:*
dtype0*
valueB:<╩
"gradients/concat_grad/ConcatOffsetConcatOffsetgradients/concat_grad/mod:z:0$gradients/concat_grad/Shape:output:0&gradients/concat_grad/Shape_1:output:0*
N* 
_output_shapes
::Л
gradients/concat_grad/SliceSlice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:0$gradients/concat_grad/Shape:output:0*
Index0*
T0*
_output_shapes
:<Н
gradients/concat_grad/Slice_1Slice&gradients/split_2_grad/concat:output:0+gradients/concat_grad/ConcatOffset:offset:1&gradients/concat_grad/Shape_1:output:0*
Index0*
T0*
_output_shapes
:<s
IdentityIdentity&gradients/transpose_grad/transpose:y:0*
T0*,
_output_shapes
:         Яt

Identity_1Identity*gradients/ExpandDims_grad/Reshape:output:0*
T0*'
_output_shapes
:         v

Identity_2Identity,gradients/ExpandDims_1_grad/Reshape:output:0*
T0*'
_output_shapes
:         e

Identity_3Identity$gradients/split_grad/concat:output:0*
T0*
_output_shapes

:<g

Identity_4Identity&gradients/split_1_grad/concat:output:0*
T0*
_output_shapes

:<c

Identity_5Identity&gradients/concat_grad/Slice_1:output:0*
T0*
_output_shapes
:<"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"!

identity_5Identity_5:output:0*(
_construction_contextkEagerRuntime*в
_input_shapes┘
о:         :         Я:         :         : :Я         ::         :         ::Я         :         :         :З::         :         : ::::::::: : : : *=
api_implements+)lstm_0067de53-6b05-422e-aca0-48144f07387a*
api_preferred_deviceGPU*A
forward_function_name(&__forward_gpu_lstm_with_fallback_74395*
go_backwards( *

time_major( :- )
'
_output_shapes
:         :2.
,
_output_shapes
:         Я:-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :2.
,
_output_shapes
:Я         : 

_output_shapes
::1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :	

_output_shapes
::2
.
,
_output_shapes
:Я         :1-
+
_output_shapes
:         :1-
+
_output_shapes
:         :!

_output_shapes	
:З: 

_output_shapes
::-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: : 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
:: 

_output_shapes
::

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
Њ:
┐
__inference_standard_lstm_75486

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4c
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          n
	transpose	Transposeinputstranspose/perm:output:0*
T0*,
_output_shapes
:Я         B
ShapeShapetranspose:y:0*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Л
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskf
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
         ▓
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмє
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Я
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУм_
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:ж
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maskd
MatMulMatMulstrided_slice_1:output:0kernel*
T0*'
_output_shapes
:         <^
MatMul_1MatMulinit_hrecurrent_kernel*
T0*'
_output_shapes
:         <d
addAddV2MatMul:product:0MatMul_1:product:0*
T0*'
_output_shapes
:         <S
BiasAddBiasAddadd:z:0bias*
T0*'
_output_shapes
:         <Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :Х
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_splitT
SigmoidSigmoidsplit:output:0*
T0*'
_output_shapes
:         V
	Sigmoid_1Sigmoidsplit:output:1*
T0*'
_output_shapes
:         S
mulMulSigmoid_1:y:0init_c*
T0*'
_output_shapes
:         N
TanhTanhsplit:output:2*
T0*'
_output_shapes
:         U
mul_1MulSigmoid:y:0Tanh:y:0*
T0*'
_output_shapes
:         T
add_1AddV2mul:z:0	mul_1:z:0*
T0*'
_output_shapes
:         V
	Sigmoid_2Sigmoidsplit:output:3*
T0*'
_output_shapes
:         K
Tanh_1Tanh	add_1:z:0*
T0*'
_output_shapes
:         Y
mul_2MulSigmoid_2:y:0
Tanh_1:y:0*
T0*'
_output_shapes
:         n
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Х
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмF
timeConst*
_output_shapes
: *
dtype0*
value	B : c
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
         T
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : «
whileStatelessWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0init_hinit_cstrided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0kernelrecurrent_kernelbias*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*`
_output_shapesN
L: : : : :         :         : : :<:<:<* 
_read_only_resource_inputs
 *
_stateful_parallelism( *
bodyR
while_body_75401*
condR
while_cond_75400*_
output_shapesN
L: : : : :         :         : : :<:<:<*
parallel_iterations Ђ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       ├
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*,
_output_shapes
:Я         *
element_dtype0h
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
         a
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Є
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          Ќ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*,
_output_shapes
:         Я[
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *  ђ?`
IdentityIdentitystrided_slice_2:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_1:y:0*
T0*,
_output_shapes
:         ЯX

Identity_2Identitywhile:output:4*
T0*'
_output_shapes
:         X

Identity_3Identitywhile:output:5*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_d78c62b6-1045-479a-bb0d-b6d9b57a4016*
api_preferred_deviceCPU*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
Њ:
┐
__inference_standard_lstm_77048

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4c
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          n
	transpose	Transposeinputstranspose/perm:output:0*
T0*,
_output_shapes
:Я         B
ShapeShapetranspose:y:0*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Л
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskf
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
         ▓
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмє
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Я
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУм_
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:ж
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maskd
MatMulMatMulstrided_slice_1:output:0kernel*
T0*'
_output_shapes
:         <^
MatMul_1MatMulinit_hrecurrent_kernel*
T0*'
_output_shapes
:         <d
addAddV2MatMul:product:0MatMul_1:product:0*
T0*'
_output_shapes
:         <S
BiasAddBiasAddadd:z:0bias*
T0*'
_output_shapes
:         <Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :Х
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_splitT
SigmoidSigmoidsplit:output:0*
T0*'
_output_shapes
:         V
	Sigmoid_1Sigmoidsplit:output:1*
T0*'
_output_shapes
:         S
mulMulSigmoid_1:y:0init_c*
T0*'
_output_shapes
:         N
TanhTanhsplit:output:2*
T0*'
_output_shapes
:         U
mul_1MulSigmoid:y:0Tanh:y:0*
T0*'
_output_shapes
:         T
add_1AddV2mul:z:0	mul_1:z:0*
T0*'
_output_shapes
:         V
	Sigmoid_2Sigmoidsplit:output:3*
T0*'
_output_shapes
:         K
Tanh_1Tanh	add_1:z:0*
T0*'
_output_shapes
:         Y
mul_2MulSigmoid_2:y:0
Tanh_1:y:0*
T0*'
_output_shapes
:         n
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Х
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмF
timeConst*
_output_shapes
: *
dtype0*
value	B : c
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
         T
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : «
whileStatelessWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0init_hinit_cstrided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0kernelrecurrent_kernelbias*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*`
_output_shapesN
L: : : : :         :         : : :<:<:<* 
_read_only_resource_inputs
 *
_stateful_parallelism( *
bodyR
while_body_76963*
condR
while_cond_76962*_
output_shapesN
L: : : : :         :         : : :<:<:<*
parallel_iterations Ђ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       ├
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*,
_output_shapes
:Я         *
element_dtype0h
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
         a
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Є
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          Ќ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*,
_output_shapes
:         Я[
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *  ђ?`
IdentityIdentitystrided_slice_2:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_1:y:0*
T0*,
_output_shapes
:         ЯX

Identity_2Identitywhile:output:4*
T0*'
_output_shapes
:         X

Identity_3Identitywhile:output:5*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_91ae4c3a-1bb4-4840-80b7-ab1926c68132*
api_preferred_deviceCPU*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
Л@
╦
(__inference_gpu_lstm_with_fallback_77640

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4ѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          v
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :                  P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : o

ExpandDims
ExpandDimsinit_hExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : s
ExpandDims_1
ExpandDimsinit_cExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : Ъ
concat_1ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0*
_output_shapes	
:ЗМ
CudnnRNNCudnnRNNtranspose:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1:output:0*
T0*f
_output_shapesT
R:                  :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          Ё
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*4
_output_shapes"
 :                  p
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         f

Identity_1Identitytranspose_9:y:0*
T0*4
_output_shapes"
 :                  Z

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*s
_input_shapesb
`:                  :         :         :<:<:<*=
api_implements+)lstm_9e12c0bd-db3f-4f45-bd70-4aa03a8d3462*
api_preferred_deviceGPU*
go_backwards( *

time_major( :\ X
4
_output_shapes"
 :                  
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
├:
┐
__inference_standard_lstm_77546

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4c
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          v
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :                  B
ShapeShapetranspose:y:0*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Л
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskf
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
         ▓
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмє
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Я
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУм_
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:ж
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maskd
MatMulMatMulstrided_slice_1:output:0kernel*
T0*'
_output_shapes
:         <^
MatMul_1MatMulinit_hrecurrent_kernel*
T0*'
_output_shapes
:         <d
addAddV2MatMul:product:0MatMul_1:product:0*
T0*'
_output_shapes
:         <S
BiasAddBiasAddadd:z:0bias*
T0*'
_output_shapes
:         <Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :Х
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_splitT
SigmoidSigmoidsplit:output:0*
T0*'
_output_shapes
:         V
	Sigmoid_1Sigmoidsplit:output:1*
T0*'
_output_shapes
:         S
mulMulSigmoid_1:y:0init_c*
T0*'
_output_shapes
:         N
TanhTanhsplit:output:2*
T0*'
_output_shapes
:         U
mul_1MulSigmoid:y:0Tanh:y:0*
T0*'
_output_shapes
:         T
add_1AddV2mul:z:0	mul_1:z:0*
T0*'
_output_shapes
:         V
	Sigmoid_2Sigmoidsplit:output:3*
T0*'
_output_shapes
:         K
Tanh_1Tanh	add_1:z:0*
T0*'
_output_shapes
:         Y
mul_2MulSigmoid_2:y:0
Tanh_1:y:0*
T0*'
_output_shapes
:         n
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Х
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмF
timeConst*
_output_shapes
: *
dtype0*
value	B : c
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
         T
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : «
whileStatelessWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0init_hinit_cstrided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0kernelrecurrent_kernelbias*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*`
_output_shapesN
L: : : : :         :         : : :<:<:<* 
_read_only_resource_inputs
 *
_stateful_parallelism( *
bodyR
while_body_77461*
condR
while_cond_77460*_
output_shapesN
L: : : : :         :         : : :<:<:<*
parallel_iterations Ђ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       ╦
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :                  *
element_dtype0h
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
         a
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Є
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          Ъ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :                  [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *  ђ?`
IdentityIdentitystrided_slice_2:output:0*
T0*'
_output_shapes
:         f

Identity_1Identitytranspose_1:y:0*
T0*4
_output_shapes"
 :                  X

Identity_2Identitywhile:output:4*
T0*'
_output_shapes
:         X

Identity_3Identitywhile:output:5*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*s
_input_shapesb
`:                  :         :         :<:<:<*=
api_implements+)lstm_9e12c0bd-db3f-4f45-bd70-4aa03a8d3462*
api_preferred_deviceCPU*
go_backwards( *

time_major( :\ X
4
_output_shapes"
 :                  
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
і
И
A__inference_lstm_3_layer_call_and_return_conditional_losses_75759

inputs.
read_readvariableop_resource:<0
read_1_readvariableop_resource:<,
read_2_readvariableop_resource:<

identity_3ѕбRead/ReadVariableOpбRead_1/ReadVariableOpбRead_2/ReadVariableOp;
ShapeShapeinputs*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Л
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskP
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :s
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:P
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    l
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:         R
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :w
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:R
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    r
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:         E
ones_like/ShapeShapeinputs*
T0*
_output_shapes
:T
ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ђ?|
	ones_likeFillones_like/Shape:output:0ones_like/Const:output:0*
T0*,
_output_shapes
:         Я]
mulMulinputsones_like:output:0*
T0*,
_output_shapes
:         Яp
Read/ReadVariableOpReadVariableOpread_readvariableop_resource*
_output_shapes

:<*
dtype0Z
IdentityIdentityRead/ReadVariableOp:value:0*
T0*
_output_shapes

:<t
Read_1/ReadVariableOpReadVariableOpread_1_readvariableop_resource*
_output_shapes

:<*
dtype0^

Identity_1IdentityRead_1/ReadVariableOp:value:0*
T0*
_output_shapes

:<p
Read_2/ReadVariableOpReadVariableOpread_2_readvariableop_resource*
_output_shapes
:<*
dtype0Z

Identity_2IdentityRead_2/ReadVariableOp:value:0*
T0*
_output_shapes
:<Х
PartitionedCallPartitionedCallmul:z:0zeros:output:0zeros_1:output:0Identity:output:0Identity_1:output:0Identity_2:output:0*
Tin

2*
Tout	
2*
_collective_manager_ids
 *g
_output_shapesU
S:         :         Я:         :         : * 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8ѓ *(
f#R!
__inference_standard_lstm_75486i

Identity_3IdentityPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         ї
NoOpNoOp^Read/ReadVariableOp^Read_1/ReadVariableOp^Read_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*1
_input_shapes 
:         Я: : : 2*
Read/ReadVariableOpRead/ReadVariableOp2.
Read_1/ReadVariableOpRead_1/ReadVariableOp2.
Read_2/ReadVariableOpRead_2/ReadVariableOp:T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs
ї	
╝
while_cond_75400
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_75400___redundant_placeholder03
/while_while_cond_75400___redundant_placeholder13
/while_while_cond_75400___redundant_placeholder23
/while_while_cond_75400___redundant_placeholder3
while_identity
`

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: K
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: ")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*S
_input_shapesB
@: : : : :         :         : ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
::

_output_shapes
::	

_output_shapes
::


_output_shapes
:
дL
Б
&__forward_gpu_lstm_with_fallback_75310

inputs
init_h_0
init_c_0

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4
cudnnrnn
transpose_9_perm

cudnnrnn_0

cudnnrnn_1

cudnnrnn_2
	transpose

expanddims
expanddims_1
concat_1
transpose_perm

init_h

init_c
concat_1_axis
transpose_1_perm
transpose_2_perm
transpose_3_perm
transpose_4_perm
transpose_5_perm
transpose_6_perm
transpose_7_perm
transpose_8_perm
split_2_split_dim
split_split_dim
split_1_split_dim
concat_axisѕc
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          B
transpose_0	Transposeinputstranspose/perm:output:0*
T0P
ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B : q

ExpandDims
ExpandDimsinit_h_0ExpandDims/dim:output:0*
T0*+
_output_shapes
:         R
ExpandDims_1/dimConst*
_output_shapes
: *
dtype0*
value	B : u
ExpandDims_1
ExpandDimsinit_c_0ExpandDims_1/dim:output:0*
T0*+
_output_shapes
:         Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ѕ
splitSplitsplit/split_dim:output:0kernel*
T0*<
_output_shapes*
(::::*
	num_splitS
split_1/split_dimConst*
_output_shapes
: *
dtype0*
value	B :ќ
split_1Splitsplit_1/split_dim:output:0recurrent_kernel*
T0*<
_output_shapes*
(::::*
	num_splitW

zeros_likeConst*
_output_shapes
:<*
dtype0*
valueB<*    M
concat/axisConst*
_output_shapes
: *
dtype0*
value	B : q
concatConcatV2zeros_like:output:0biasconcat/axis:output:0*
N*
T0*
_output_shapes
:xS
split_2/split_dimConst*
_output_shapes
: *
dtype0*
value	B : Ю
split_2Splitsplit_2/split_dim:output:0concat:output:0*
T0*D
_output_shapes2
0::::::::*
	num_splitX
ConstConst*
_output_shapes
:*
dtype0*
valueB:
         a
transpose_1/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_1	Transposesplit:output:0transpose_1/perm:output:0*
T0*
_output_shapes

:X
ReshapeReshapetranspose_1:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_2/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_2	Transposesplit:output:1transpose_2/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_1Reshapetranspose_2:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_3/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_3	Transposesplit:output:2transpose_3/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_2Reshapetranspose_3:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_4/permConst*
_output_shapes
:*
dtype0*
valueB"       l
transpose_4	Transposesplit:output:3transpose_4/perm:output:0*
T0*
_output_shapes

:Z
	Reshape_3Reshapetranspose_4:y:0Const:output:0*
T0*
_output_shapes
:a
transpose_5/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_5	Transposesplit_1:output:0transpose_5/perm:output:0*
T0*
_output_shapes

:[
	Reshape_4Reshapetranspose_5:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_6/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_6	Transposesplit_1:output:1transpose_6/perm:output:0*
T0*
_output_shapes

:[
	Reshape_5Reshapetranspose_6:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_7/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_7	Transposesplit_1:output:2transpose_7/perm:output:0*
T0*
_output_shapes

:[
	Reshape_6Reshapetranspose_7:y:0Const:output:0*
T0*
_output_shapes	
:рa
transpose_8/permConst*
_output_shapes
:*
dtype0*
valueB"       n
transpose_8	Transposesplit_1:output:3transpose_8/perm:output:0*
T0*
_output_shapes

:[
	Reshape_7Reshapetranspose_8:y:0Const:output:0*
T0*
_output_shapes	
:р[
	Reshape_8Reshapesplit_2:output:0Const:output:0*
T0*
_output_shapes
:[
	Reshape_9Reshapesplit_2:output:1Const:output:0*
T0*
_output_shapes
:\

Reshape_10Reshapesplit_2:output:2Const:output:0*
T0*
_output_shapes
:\

Reshape_11Reshapesplit_2:output:3Const:output:0*
T0*
_output_shapes
:\

Reshape_12Reshapesplit_2:output:4Const:output:0*
T0*
_output_shapes
:\

Reshape_13Reshapesplit_2:output:5Const:output:0*
T0*
_output_shapes
:\

Reshape_14Reshapesplit_2:output:6Const:output:0*
T0*
_output_shapes
:\

Reshape_15Reshapesplit_2:output:7Const:output:0*
T0*
_output_shapes
:O
concat_1/axisConst*
_output_shapes
: *
dtype0*
value	B : ё

concat_1_0ConcatV2Reshape:output:0Reshape_1:output:0Reshape_2:output:0Reshape_3:output:0Reshape_4:output:0Reshape_5:output:0Reshape_6:output:0Reshape_7:output:0Reshape_8:output:0Reshape_9:output:0Reshape_10:output:0Reshape_11:output:0Reshape_12:output:0Reshape_13:output:0Reshape_14:output:0Reshape_15:output:0concat_1/axis:output:0*
N*
T0О
CudnnRNNCudnnRNNtranspose_0:y:0ExpandDims:output:0ExpandDims_1:output:0concat_1_0:output:0*
T0*f
_output_shapesT
R:                  :         :         :f
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:
         _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:т
strided_sliceStridedSliceCudnnRNN:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_9/permConst*
_output_shapes
:*
dtype0*!
valueB"          Ё
transpose_9	TransposeCudnnRNN:output:0transpose_9/perm:output:0*
T0*4
_output_shapes"
 :                  p
SqueezeSqueezeCudnnRNN:output_h:0*
T0*'
_output_shapes
:         *
squeeze_dims
 r
	Squeeze_1SqueezeCudnnRNN:output_c:0*
T0*'
_output_shapes
:         *
squeeze_dims
 [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *   @^
IdentityIdentitystrided_slice:output:0*
T0*'
_output_shapes
:         f

Identity_1Identitytranspose_9:y:0*
T0*4
_output_shapes"
 :                  Z

Identity_2IdentitySqueeze:output:0*
T0*'
_output_shapes
:         \

Identity_3IdentitySqueeze_1:output:0*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
concat_1concat_1_0:output:0"'
concat_1_axisconcat_1/axis:output:0"#
concat_axisconcat/axis:output:0"
cudnnrnnCudnnRNN:output:0"!

cudnnrnn_0CudnnRNN:output_h:0"!

cudnnrnn_1CudnnRNN:output_c:0"&

cudnnrnn_2CudnnRNN:reserve_space:0"!

expanddimsExpandDims:output:0"%
expanddims_1ExpandDims_1:output:0"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"
init_cinit_c_0"
init_hinit_h_0"/
split_1_split_dimsplit_1/split_dim:output:0"/
split_2_split_dimsplit_2/split_dim:output:0"+
split_split_dimsplit/split_dim:output:0"
	transposetranspose_0:y:0"-
transpose_1_permtranspose_1/perm:output:0"-
transpose_2_permtranspose_2/perm:output:0"-
transpose_3_permtranspose_3/perm:output:0"-
transpose_4_permtranspose_4/perm:output:0"-
transpose_5_permtranspose_5/perm:output:0"-
transpose_6_permtranspose_6/perm:output:0"-
transpose_7_permtranspose_7/perm:output:0"-
transpose_8_permtranspose_8/perm:output:0"-
transpose_9_permtranspose_9/perm:output:0")
transpose_permtranspose/perm:output:0*(
_construction_contextkEagerRuntime*s
_input_shapesb
`:                  :         :         :<:<:<*=
api_implements+)lstm_2ce075de-0b33-47d3-b5ef-91fa50ccaa52*
api_preferred_deviceGPU*U
backward_function_name;9__inference___backward_gpu_lstm_with_fallback_75135_75311*
go_backwards( *

time_major( :\ X
4
_output_shapes"
 :                  
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
к(
╬
while_body_74955
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0
while_matmul_kernel_0%
!while_matmul_1_recurrent_kernel_0
while_biasadd_bias_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor
while_matmul_kernel#
while_matmul_1_recurrent_kernel
while_biasadd_biasѕ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       д
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:         *
element_dtype0Љ
while/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0while_matmul_kernel_0*
T0*'
_output_shapes
:         <ѓ
while/MatMul_1MatMulwhile_placeholder_2!while_matmul_1_recurrent_kernel_0*
T0*'
_output_shapes
:         <v
	while/addAddV2while/MatMul:product:0while/MatMul_1:product:0*
T0*'
_output_shapes
:         <o
while/BiasAddBiasAddwhile/add:z:0while_biasadd_bias_0*
T0*'
_output_shapes
:         <W
while/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :╚
while/splitSplitwhile/split/split_dim:output:0while/BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_split`
while/SigmoidSigmoidwhile/split:output:0*
T0*'
_output_shapes
:         b
while/Sigmoid_1Sigmoidwhile/split:output:1*
T0*'
_output_shapes
:         l
	while/mulMulwhile/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:         Z

while/TanhTanhwhile/split:output:2*
T0*'
_output_shapes
:         g
while/mul_1Mulwhile/Sigmoid:y:0while/Tanh:y:0*
T0*'
_output_shapes
:         f
while/add_1AddV2while/mul:z:0while/mul_1:z:0*
T0*'
_output_shapes
:         b
while/Sigmoid_2Sigmoidwhile/split:output:3*
T0*'
_output_shapes
:         W
while/Tanh_1Tanhwhile/add_1:z:0*
T0*'
_output_shapes
:         k
while/mul_2Mulwhile/Sigmoid_2:y:0while/Tanh_1:y:0*
T0*'
_output_shapes
:         И
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/mul_2:z:0*
_output_shapes
: *
element_dtype0:жУмO
while/add_2/yConst*
_output_shapes
: *
dtype0*
value	B :`
while/add_2AddV2while_placeholderwhile/add_2/y:output:0*
T0*
_output_shapes
: O
while/add_3/yConst*
_output_shapes
: *
dtype0*
value	B :g
while/add_3AddV2while_while_loop_counterwhile/add_3/y:output:0*
T0*
_output_shapes
: L
while/IdentityIdentitywhile/add_3:z:0*
T0*
_output_shapes
: ]
while/Identity_1Identitywhile_while_maximum_iterations*
T0*
_output_shapes
: N
while/Identity_2Identitywhile/add_2:z:0*
T0*
_output_shapes
: ї
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*
T0*
_output_shapes
: :жУм_
while/Identity_4Identitywhile/mul_2:z:0*
T0*'
_output_shapes
:         _
while/Identity_5Identitywhile/add_1:z:0*
T0*'
_output_shapes
:         "*
while_biasadd_biaswhile_biasadd_bias_0")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"D
while_matmul_1_recurrent_kernel!while_matmul_1_recurrent_kernel_0",
while_matmul_kernelwhile_matmul_kernel_0",
while_strided_slicewhile_strided_slice_0"е
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*_
_input_shapesN
L: : : : :         :         : : :<:<:<: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:         :-)
'
_output_shapes
:         :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:<:$	 

_output_shapes

:<: 


_output_shapes
:<
І
▓
&__inference_lstm_3_layer_call_fn_77366
inputs_0
unknown:<
	unknown_0:<
	unknown_1:<
identityѕбStatefulPartitionedCallУ
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:         *%
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8ѓ *J
fERC
A__inference_lstm_3_layer_call_and_return_conditional_losses_75313o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:         `
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:                  : : : 22
StatefulPartitionedCallStatefulPartitionedCall:^ Z
4
_output_shapes"
 :                  
"
_user_specified_name
inputs/0
├:
┐
__inference_standard_lstm_75040

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4c
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          v
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :                  B
ShapeShapetranspose:y:0*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Л
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskf
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
         ▓
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмє
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Я
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУм_
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:ж
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maskd
MatMulMatMulstrided_slice_1:output:0kernel*
T0*'
_output_shapes
:         <^
MatMul_1MatMulinit_hrecurrent_kernel*
T0*'
_output_shapes
:         <d
addAddV2MatMul:product:0MatMul_1:product:0*
T0*'
_output_shapes
:         <S
BiasAddBiasAddadd:z:0bias*
T0*'
_output_shapes
:         <Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :Х
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_splitT
SigmoidSigmoidsplit:output:0*
T0*'
_output_shapes
:         V
	Sigmoid_1Sigmoidsplit:output:1*
T0*'
_output_shapes
:         S
mulMulSigmoid_1:y:0init_c*
T0*'
_output_shapes
:         N
TanhTanhsplit:output:2*
T0*'
_output_shapes
:         U
mul_1MulSigmoid:y:0Tanh:y:0*
T0*'
_output_shapes
:         T
add_1AddV2mul:z:0	mul_1:z:0*
T0*'
_output_shapes
:         V
	Sigmoid_2Sigmoidsplit:output:3*
T0*'
_output_shapes
:         K
Tanh_1Tanh	add_1:z:0*
T0*'
_output_shapes
:         Y
mul_2MulSigmoid_2:y:0
Tanh_1:y:0*
T0*'
_output_shapes
:         n
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Х
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмF
timeConst*
_output_shapes
: *
dtype0*
value	B : c
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
         T
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : «
whileStatelessWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0init_hinit_cstrided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0kernelrecurrent_kernelbias*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*`
_output_shapesN
L: : : : :         :         : : :<:<:<* 
_read_only_resource_inputs
 *
_stateful_parallelism( *
bodyR
while_body_74955*
condR
while_cond_74954*_
output_shapesN
L: : : : :         :         : : :<:<:<*
parallel_iterations Ђ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       ╦
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :                  *
element_dtype0h
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
         a
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Є
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          Ъ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :                  [
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *  ђ?`
IdentityIdentitystrided_slice_2:output:0*
T0*'
_output_shapes
:         f

Identity_1Identitytranspose_1:y:0*
T0*4
_output_shapes"
 :                  X

Identity_2Identitywhile:output:4*
T0*'
_output_shapes
:         X

Identity_3Identitywhile:output:5*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*s
_input_shapesb
`:                  :         :         :<:<:<*=
api_implements+)lstm_2ce075de-0b33-47d3-b5ef-91fa50ccaa52*
api_preferred_deviceCPU*
go_backwards( *

time_major( :\ X
4
_output_shapes"
 :                  
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias
Њ:
┐
__inference_standard_lstm_78440

inputs

init_h

init_c

kernel
recurrent_kernel
bias
identity

identity_1

identity_2

identity_3

identity_4c
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          n
	transpose	Transposeinputstranspose/perm:output:0*
T0*,
_output_shapes
:Я         B
ShapeShapetranspose:y:0*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Л
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskf
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
         ▓
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмє
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Я
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУм_
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:ж
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maskd
MatMulMatMulstrided_slice_1:output:0kernel*
T0*'
_output_shapes
:         <^
MatMul_1MatMulinit_hrecurrent_kernel*
T0*'
_output_shapes
:         <d
addAddV2MatMul:product:0MatMul_1:product:0*
T0*'
_output_shapes
:         <S
BiasAddBiasAddadd:z:0bias*
T0*'
_output_shapes
:         <Q
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :Х
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*`
_output_shapesN
L:         :         :         :         *
	num_splitT
SigmoidSigmoidsplit:output:0*
T0*'
_output_shapes
:         V
	Sigmoid_1Sigmoidsplit:output:1*
T0*'
_output_shapes
:         S
mulMulSigmoid_1:y:0init_c*
T0*'
_output_shapes
:         N
TanhTanhsplit:output:2*
T0*'
_output_shapes
:         U
mul_1MulSigmoid:y:0Tanh:y:0*
T0*'
_output_shapes
:         T
add_1AddV2mul:z:0	mul_1:z:0*
T0*'
_output_shapes
:         V
	Sigmoid_2Sigmoidsplit:output:3*
T0*'
_output_shapes
:         K
Tanh_1Tanh	add_1:z:0*
T0*'
_output_shapes
:         Y
mul_2MulSigmoid_2:y:0
Tanh_1:y:0*
T0*'
_output_shapes
:         n
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       Х
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type0:жУмF
timeConst*
_output_shapes
: *
dtype0*
value	B : c
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
         T
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : «
whileStatelessWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0init_hinit_cstrided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0kernelrecurrent_kernelbias*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*`
_output_shapesN
L: : : : :         :         : : :<:<:<* 
_read_only_resource_inputs
 *
_stateful_parallelism( *
bodyR
while_body_78355*
condR
while_cond_78354*_
output_shapesN
L: : : : :         :         : : :<:<:<*
parallel_iterations Ђ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"       ├
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*,
_output_shapes
:Я         *
element_dtype0h
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
         a
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: a
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Є
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:         *
shrink_axis_maske
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          Ќ
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*,
_output_shapes
:         Я[
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *  ђ?`
IdentityIdentitystrided_slice_2:output:0*
T0*'
_output_shapes
:         ^

Identity_1Identitytranspose_1:y:0*
T0*,
_output_shapes
:         ЯX

Identity_2Identitywhile:output:4*
T0*'
_output_shapes
:         X

Identity_3Identitywhile:output:5*
T0*'
_output_shapes
:         I

Identity_4Identityruntime:output:0*
T0*
_output_shapes
: "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0*(
_construction_contextkEagerRuntime*k
_input_shapesZ
X:         Я:         :         :<:<:<*=
api_implements+)lstm_c1189ae0-4eec-42a2-962a-5abd558aa8a4*
api_preferred_deviceCPU*
go_backwards( *

time_major( :T P
,
_output_shapes
:         Я
 
_user_specified_nameinputs:OK
'
_output_shapes
:         
 
_user_specified_nameinit_h:OK
'
_output_shapes
:         
 
_user_specified_nameinit_c:FB

_output_shapes

:<
 
_user_specified_namekernel:PL

_output_shapes

:<
*
_user_specified_namerecurrent_kernel:@<

_output_shapes
:<

_user_specified_namebias"█L
saver_filename:0StatefulPartitionedCall_1:0StatefulPartitionedCall_28"
saved_model_main_op

NoOp*>
__saved_model_init_op%#
__saved_model_init_op

NoOp*╣
serving_defaultЦ
J
lstm_3_input:
serving_default_lstm_3_input:0         Я;
dense_30
StatefulPartitionedCall:0         tensorflow/serving/predict:ЗW
┤
layer_with_weights-0
layer-0
layer_with_weights-1
layer-1
	optimizer
	variables
trainable_variables
regularization_losses
	keras_api
__call__
*	&call_and_return_all_conditional_losses

_default_save_signature

signatures"
_tf_keras_sequential
┌
cell

state_spec
	variables
trainable_variables
regularization_losses
	keras_api
_random_generator
__call__
*&call_and_return_all_conditional_losses"
_tf_keras_rnn_layer
╗

kernel
bias
	variables
trainable_variables
regularization_losses
	keras_api
__call__
*&call_and_return_all_conditional_losses"
_tf_keras_layer
Г
iter

beta_1

beta_2
	 decay
!learning_ratemHmI"mJ#mK$mLvMvN"vO#vP$vQ"
	optimizer
C
"0
#1
$2
3
4"
trackable_list_wrapper
C
"0
#1
$2
3
4"
trackable_list_wrapper
 "
trackable_list_wrapper
╩
%non_trainable_variables

&layers
'metrics
(layer_regularization_losses
)layer_metrics
	variables
trainable_variables
regularization_losses
__call__

_default_save_signature
*	&call_and_return_all_conditional_losses
&	"call_and_return_conditional_losses"
_generic_user_object
■2ч
,__inference_sequential_3_layer_call_fn_75797
,__inference_sequential_3_layer_call_fn_76406
,__inference_sequential_3_layer_call_fn_76421
,__inference_sequential_3_layer_call_fn_76353└
и▓│
FullArgSpec1
args)џ&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaultsџ
p 

 

kwonlyargsџ 
kwonlydefaultsф 
annotationsф *
 
Ж2у
G__inference_sequential_3_layer_call_and_return_conditional_losses_76858
G__inference_sequential_3_layer_call_and_return_conditional_losses_77327
G__inference_sequential_3_layer_call_and_return_conditional_losses_76369
G__inference_sequential_3_layer_call_and_return_conditional_losses_76385└
и▓│
FullArgSpec1
args)џ&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaultsџ
p 

 

kwonlyargsџ 
kwonlydefaultsф 
annotationsф *
 
лB═
 __inference__wrapped_model_74404lstm_3_input"ў
Љ▓Ї
FullArgSpec
argsџ 
varargsjargs
varkwjkwargs
defaults
 

kwonlyargsџ 
kwonlydefaults
 
annotationsф *
 
,
*serving_default"
signature_map
Э
+
state_size

"kernel
#recurrent_kernel
$bias
,	variables
-trainable_variables
.regularization_losses
/	keras_api
0_random_generator
1__call__
*2&call_and_return_all_conditional_losses"
_tf_keras_layer
 "
trackable_list_wrapper
5
"0
#1
$2"
trackable_list_wrapper
5
"0
#1
$2"
trackable_list_wrapper
 "
trackable_list_wrapper
╣

3states
4non_trainable_variables

5layers
6metrics
7layer_regularization_losses
8layer_metrics
	variables
trainable_variables
regularization_losses
__call__
*&call_and_return_all_conditional_losses
&"call_and_return_conditional_losses"
_generic_user_object
"
_generic_user_object
ч2Э
&__inference_lstm_3_layer_call_fn_77355
&__inference_lstm_3_layer_call_fn_77366
&__inference_lstm_3_layer_call_fn_77377
&__inference_lstm_3_layer_call_fn_77388Н
╠▓╚
FullArgSpecB
args:џ7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaultsџ

 
p 

 

kwonlyargsџ 
kwonlydefaultsф 
annotationsф *
 
у2С
A__inference_lstm_3_layer_call_and_return_conditional_losses_77819
A__inference_lstm_3_layer_call_and_return_conditional_losses_78282
A__inference_lstm_3_layer_call_and_return_conditional_losses_78713
A__inference_lstm_3_layer_call_and_return_conditional_losses_79176Н
╠▓╚
FullArgSpecB
args:џ7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaultsџ

 
p 

 

kwonlyargsџ 
kwonlydefaultsф 
annotationsф *
 
 :2dense_3/kernel
:2dense_3/bias
.
0
1"
trackable_list_wrapper
.
0
1"
trackable_list_wrapper
 "
trackable_list_wrapper
Г
9non_trainable_variables

:layers
;metrics
<layer_regularization_losses
=layer_metrics
	variables
trainable_variables
regularization_losses
__call__
*&call_and_return_all_conditional_losses
&"call_and_return_conditional_losses"
_generic_user_object
Л2╬
'__inference_dense_3_layer_call_fn_79185б
Ў▓Ћ
FullArgSpec
argsџ
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargsџ 
kwonlydefaults
 
annotationsф *
 
В2ж
B__inference_dense_3_layer_call_and_return_conditional_losses_79195б
Ў▓Ћ
FullArgSpec
argsџ
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargsџ 
kwonlydefaults
 
annotationsф *
 
:	 (2	Adam/iter
: (2Adam/beta_1
: (2Adam/beta_2
: (2
Adam/decay
: (2Adam/learning_rate
+:)<2lstm_3/lstm_cell_3/kernel
5:3<2#lstm_3/lstm_cell_3/recurrent_kernel
%:#<2lstm_3/lstm_cell_3/bias
 "
trackable_list_wrapper
.
0
1"
trackable_list_wrapper
'
>0"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
¤B╠
#__inference_signature_wrapper_77344lstm_3_input"ћ
Ї▓Ѕ
FullArgSpec
argsџ 
varargs
 
varkwjkwargs
defaults
 

kwonlyargsџ 
kwonlydefaults
 
annotationsф *
 
 "
trackable_list_wrapper
5
"0
#1
$2"
trackable_list_wrapper
5
"0
#1
$2"
trackable_list_wrapper
 "
trackable_list_wrapper
Г
?non_trainable_variables

@layers
Ametrics
Blayer_regularization_losses
Clayer_metrics
,	variables
-trainable_variables
.regularization_losses
1__call__
*2&call_and_return_all_conditional_losses
&2"call_and_return_conditional_losses"
_generic_user_object
"
_generic_user_object
─2┴Й
х▓▒
FullArgSpec3
args+џ(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaultsџ
p 

kwonlyargsџ 
kwonlydefaultsф 
annotationsф *
 
─2┴Й
х▓▒
FullArgSpec3
args+џ(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaultsџ
p 

kwonlyargsџ 
kwonlydefaultsф 
annotationsф *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
'
0"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
N
	Dtotal
	Ecount
F	variables
G	keras_api"
_tf_keras_metric
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
:  (2total
:  (2count
.
D0
E1"
trackable_list_wrapper
-
F	variables"
_generic_user_object
%:#2Adam/dense_3/kernel/m
:2Adam/dense_3/bias/m
0:.<2 Adam/lstm_3/lstm_cell_3/kernel/m
::8<2*Adam/lstm_3/lstm_cell_3/recurrent_kernel/m
*:(<2Adam/lstm_3/lstm_cell_3/bias/m
%:#2Adam/dense_3/kernel/v
:2Adam/dense_3/bias/v
0:.<2 Adam/lstm_3/lstm_cell_3/kernel/v
::8<2*Adam/lstm_3/lstm_cell_3/recurrent_kernel/v
*:(<2Adam/lstm_3/lstm_cell_3/bias/vџ
 __inference__wrapped_model_74404v"#$:б7
0б-
+і(
lstm_3_input         Я
ф "1ф.
,
dense_3!і
dense_3         б
B__inference_dense_3_layer_call_and_return_conditional_losses_79195\/б,
%б"
 і
inputs         
ф "%б"
і
0         
џ z
'__inference_dense_3_layer_call_fn_79185O/б,
%б"
 і
inputs         
ф "і         ┬
A__inference_lstm_3_layer_call_and_return_conditional_losses_77819}"#$OбL
EбB
4џ1
/і,
inputs/0                  

 
p 

 
ф "%б"
і
0         
џ ┬
A__inference_lstm_3_layer_call_and_return_conditional_losses_78282}"#$OбL
EбB
4џ1
/і,
inputs/0                  

 
p

 
ф "%б"
і
0         
џ │
A__inference_lstm_3_layer_call_and_return_conditional_losses_78713n"#$@б=
6б3
%і"
inputs         Я

 
p 

 
ф "%б"
і
0         
џ │
A__inference_lstm_3_layer_call_and_return_conditional_losses_79176n"#$@б=
6б3
%і"
inputs         Я

 
p

 
ф "%б"
і
0         
џ џ
&__inference_lstm_3_layer_call_fn_77355p"#$OбL
EбB
4џ1
/і,
inputs/0                  

 
p 

 
ф "і         џ
&__inference_lstm_3_layer_call_fn_77366p"#$OбL
EбB
4џ1
/і,
inputs/0                  

 
p

 
ф "і         І
&__inference_lstm_3_layer_call_fn_77377a"#$@б=
6б3
%і"
inputs         Я

 
p 

 
ф "і         І
&__inference_lstm_3_layer_call_fn_77388a"#$@б=
6б3
%і"
inputs         Я

 
p

 
ф "і         й
G__inference_sequential_3_layer_call_and_return_conditional_losses_76369r"#$Bб?
8б5
+і(
lstm_3_input         Я
p 

 
ф "%б"
і
0         
џ й
G__inference_sequential_3_layer_call_and_return_conditional_losses_76385r"#$Bб?
8б5
+і(
lstm_3_input         Я
p

 
ф "%б"
і
0         
џ и
G__inference_sequential_3_layer_call_and_return_conditional_losses_76858l"#$<б9
2б/
%і"
inputs         Я
p 

 
ф "%б"
і
0         
џ и
G__inference_sequential_3_layer_call_and_return_conditional_losses_77327l"#$<б9
2б/
%і"
inputs         Я
p

 
ф "%б"
і
0         
џ Ћ
,__inference_sequential_3_layer_call_fn_75797e"#$Bб?
8б5
+і(
lstm_3_input         Я
p 

 
ф "і         Ћ
,__inference_sequential_3_layer_call_fn_76353e"#$Bб?
8б5
+і(
lstm_3_input         Я
p

 
ф "і         Ј
,__inference_sequential_3_layer_call_fn_76406_"#$<б9
2б/
%і"
inputs         Я
p 

 
ф "і         Ј
,__inference_sequential_3_layer_call_fn_76421_"#$<б9
2б/
%і"
inputs         Я
p

 
ф "і         «
#__inference_signature_wrapper_77344є"#$JбG
б 
@ф=
;
lstm_3_input+і(
lstm_3_input         Я"1ф.
,
dense_3!і
dense_3         