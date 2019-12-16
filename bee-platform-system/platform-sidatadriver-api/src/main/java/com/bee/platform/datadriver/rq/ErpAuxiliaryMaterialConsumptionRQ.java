package com.bee.platform.datadriver.rq;


import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
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
@ApiModel("辅材消耗请求参数")
public class ErpAuxiliaryMaterialConsumptionRQ implements Serializable {

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
    @NotNull(message = "公司不能为空")
    private Integer companyId;
    /**
     * 公司名称
     */
    @ApiModelProperty("公司名称")
//    @NotEmpty(message = "公司不能为空")
    private String companyName;
    /**
     * 创建时间
     */
    @NotNull(message = "创建时间不能为空")
    @ApiModelProperty("创建时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date consumptionDate;

    @NotNull(message = "炉号不能为空")
    @ApiModelProperty("炉号id")
    private Integer furnaceId;
    /**
     * 炉号
     */
    @ApiModelProperty("炉号")
//    @NotEmpty(message = "炉号不能为空")
    private String furnaceNumber;
    /**
     * 产量
     */
//    @ApiModelProperty("产量")
//    private BigDecimal output;
    /**
     * 生产领料金额
     */
    @ApiModelProperty("生产领料金额")
    @Min(value = 0,message = "金额不能小于0")
    private BigDecimal productionMoney;
    /**
     * 环保金额
     */
    @ApiModelProperty("环保金额")
    @Min(value = 0,message = "金额不能小于0")
    private BigDecimal environmentalMoney;
    /**
     * 安全金额
     */
    @ApiModelProperty("安全金额")
    @Min(value = 0,message = "金额不能小于0")
    private BigDecimal securityMoney;
    /**
     * 检修金额
     */
    @ApiModelProperty("检修金额")
    @Min(value = 0,message = "金额不能小于0")
    private BigDecimal overhaulMoney;





}
