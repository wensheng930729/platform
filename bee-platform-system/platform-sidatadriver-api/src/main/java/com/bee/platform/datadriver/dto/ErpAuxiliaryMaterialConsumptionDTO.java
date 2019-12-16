package com.bee.platform.datadriver.dto;


import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * <p>
 * 辅材消耗表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("辅材消耗列表返回信息123")
@JsonInclude
public class ErpAuxiliaryMaterialConsumptionDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 公司id
     */
    @ApiModelProperty("公司id")
    private Integer companyId;
    /**
     * 公司名称
     */
    @ApiModelProperty("公司名称")
    private String companyName;

    /**
     * 辅材消耗时间
     */
    @ApiModelProperty("辅材消耗时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date consumptionDate;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    private Date createTime;
    /**
     * 炉号
     */
    @ApiModelProperty("炉号")
    private String furnaceNumber; /**
     * 炉号
     */
    @ApiModelProperty("炉号id")
    private Integer furnaceId;
    /**
     * 产量
     */
    @ApiModelProperty("产量")
    private BigDecimal output;
    /**
     * 生产领料金额
     */
    @ApiModelProperty("生产领料金额")
    private BigDecimal productionMoney;
    /**
     * 环保金额
     */
    @ApiModelProperty("环保金额")
    private BigDecimal environmentalMoney;
    /**
     * 安全金额
     */
    @ApiModelProperty("安全金额")
    private BigDecimal securityMoney;
    /**
     * 检修金额
     */
    @ApiModelProperty("检修金额")
    private BigDecimal overhaulMoney;





}
