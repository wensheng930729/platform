package com.bee.platform.customer.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "联系人增加相关的")
public class AuthContactAddRQ implements Serializable {


    /**
	 * 
	 */
	private static final long serialVersionUID = 389189534709349362L;

	@ApiModelProperty("联系人二级分类")
    private String secondType;

    @ApiModelProperty("联系人姓名")
    @NotEmpty(message = "姓名不能为空")
    private String name;

    @ApiModelProperty("性别")
    private String sex;

    @ApiModelProperty("电话号码")
    @NotEmpty(message = "电话号码不能为空")
    private String phone;

    @ApiModelProperty("座机")
    private String fixtel;

    @ApiModelProperty("生日")
    private String birthday;

    @ApiModelProperty("联系人的邮寄地址")
    private String address;

    @ApiModelProperty("爱好")
    private String hobby;

    @ApiModelProperty("联系人星级评定：1是一星 2是二星 3是三星 4是四星 5是五星")
    @NotNull(message = "联系人星级评定不能为空")
    private Integer starLevel;

    @ApiModelProperty("工作简介")
    private String workBrief;


}
