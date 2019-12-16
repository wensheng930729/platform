package com.bee.platform.customer.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "客户添加相关dto")
public class AuthCustomerAddRQ implements Serializable {

    /**
	 * 
	 */
	private static final long serialVersionUID = 6415042885559111978L;

	@ApiModelProperty("企业ID")
    @NotNull(message = "企业不能为空")
    private Integer enterpriseId;

    @NotEmpty(message = "客户姓名不能为空")
    @ApiModelProperty("客户姓名")
    private String cusName;

    @ApiModelProperty("客户简称")
    private String simpleName;

    @NotEmpty(message = "一级分类不能为空")
    @ApiModelProperty("客户一级分类")
    private List<String> cusFirstType;

    @ApiModelProperty("客户二级分类")
    @NotEmpty(message = "二级分类不能为空")
    private List<Integer> cusSecondType;

    @NotNull(message = "用户状态不能为空")
    @ApiModelProperty("用户状态：1启用 0禁用")
    private Integer status;

    @ApiModelProperty("联系人列表")
    private List<AuthContactAddRQ> contactList;

}
