package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName EnterpriseSearchDTO
 * @Description 功能描述
 * @Date 2019/5/10 16:35
 **/


@Data
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("模糊搜索企业列表返回信息")
public class EnterpriseSearchDTO implements Serializable{

    /**
	 * 
	 */
	private static final long serialVersionUID = -3460490419158643883L;

	@ApiModelProperty("企业id")
    private Integer id;

    @ApiModelProperty("企业名称")
    private String name;

    @ApiModelProperty("地址")
    private String address;


    @ApiModelProperty("认证通过时间")
    private Date createAt;



}
