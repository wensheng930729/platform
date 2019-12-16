package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 操作日志表
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("erp操作日志的dto")
@JsonInclude
public class ErpOperationLogDTO implements Serializable {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("业务类型")
    private String businessType;
    /**
     * 操作人
     */
    @ApiModelProperty("操作人id")
    private Integer operator;
    
    /**
     * 操作人姓名
     */
    @ApiModelProperty("操作人姓名")
    private String operatorName;
    /**
     * 操作说明
     */
    @ApiModelProperty("操作说明")
    private String operateMsg;
    /**
     * 操作时间
     */
    @ApiModelProperty("操作时间")
    private Date operateTime;


}
