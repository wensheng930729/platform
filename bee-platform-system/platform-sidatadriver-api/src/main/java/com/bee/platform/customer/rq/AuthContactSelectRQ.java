package com.bee.platform.customer.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.Digits;
import java.io.Serializable;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "联系人相关的")
public class AuthContactSelectRQ implements Serializable {

    /**
	 * 
	 */
	private static final long serialVersionUID = 6130799771151998345L;

	@ApiModelProperty("联系人编号")
    private String contactNo;

    @ApiModelProperty("二级分类")
    private String secondType;

    @ApiModelProperty("联系人姓名")
    private String name;

    @ApiModelProperty("电话号码")
    @Digits(integer = 11 ,fraction = 0 ,message = "输入正确的手机好号码")
    private String phone;

    @ApiModelProperty("联系人星级评定：1是一星 2是二星 3是三星 4是四星 5是五星")
    private Integer starLevel;
}
