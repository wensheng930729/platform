package com.bee.platform.user.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @author chengke
 * @version 1.0.0
 * @ClassName UserApplyEnterprisesDTO
 * @Description 用户申请企业记录信息
 * @Date 2019/4/30 16:01
 **/
@Data
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("用户申请企业记录信息")
@JsonInclude
public class UserApplyEnterprisesDTO implements Serializable{


    /**
	 * 
	 */
	private static final long serialVersionUID = 4774030652528515480L;
	/**
     * id
     */
    @ApiModelProperty(value = "企业审核记录id")
    private Integer id;
    /**
     * 企业的名称
     */
    @ApiModelProperty(value = "企业名称")
    private String name;

    @ApiModelProperty(value = "待审核企业的状态(0: 未通过，1: 已通过, 2: 审核中)")
    private Integer type;

    /**
     * 创建日期
     */
    @ApiModelProperty(value = "创建日期")
    private Date createAt;

    /**
     * 审核失败原因
     */
    @ApiModelProperty(value = "审核失败原因")
    private String failureReason;


}
