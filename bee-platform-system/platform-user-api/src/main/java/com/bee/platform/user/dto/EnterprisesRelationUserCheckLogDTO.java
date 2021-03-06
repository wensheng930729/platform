package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @author cheng.ke
 * @version 1.0.0
 * @ClassName EnterprisesRelationUserCheckLogDTO
 * @Description 企业关联用户审核日志返回信息
 * @Date 2019/4/30 16:40
 **/

@Data
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("企业关联用户审核日志返回信息")
public class EnterprisesRelationUserCheckLogDTO implements Serializable{

    

    /**
	 * 
	 */
	private static final long serialVersionUID = 5110771668654401947L;
	/**
     * 企业名称
     */
    @ApiModelProperty("企业名称")
    private String enterpriseName;
    /**
     * 操作人名称
     */
    @ApiModelProperty("操作人名称")
    private String operateName;
    /**
     * 操作类型(0审核申请，1申请入住)
     */
    @ApiModelProperty("操作类型(0审核申请，1申请入住)")
    private Integer operateType;
    /**
     * 执行结果（0未通过，1已通过，2未审核）
     */
    @ApiModelProperty("执行结果（0未通过，1已通过，2未审核）")
    private Integer operateResult;
    /**
     * 操作时间
     */
    @ApiModelProperty("操作时间")
    private Date operateTime;
    /**
     * 拒绝原因
     */
    @ApiModelProperty("拒绝原因")
    private String refuseReason;


}
