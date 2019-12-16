package com.bee.platform.customer.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.List;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "查询客户详情的dto")
public class AuthCustomerDetailDTO implements Serializable {

    /**
	 * 
	 */
	private static final long serialVersionUID = -3766394795036338860L;

	@ApiModelProperty("客户id")
    private Integer id;

    @ApiModelProperty("客户编码")
    private String cusNo;

    @ApiModelProperty("企业ID")
    private Integer enterpriseId;

    @NotEmpty(message = "客户姓名不能为空")
    private String cusName;

    @ApiModelProperty("客户简称")
    private String simpleName;

    @NotEmpty(message = "一级分类不能为空")
    private List<String> cusFirstType;

    @ApiModelProperty("客户二级分类")
    private List<Integer> cusSecondType;

    @ApiModelProperty("用户状态：1启用 0禁用")
    private Integer status;

    @ApiModelProperty("联系人列表")
    private List<AuthContactDto> contactList;

}
