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
import java.util.List;

/**
 * <p>
 * 领料出库主表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("领料出库返回信息")
@JsonInclude
public class ErpOutOfStockOrderDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 领料出库编号
     */
    @ApiModelProperty("领料出库编号")
    private String code;
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
     * 出库日期
     */
    @ApiModelProperty("出库日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date outOfStockTime;
    /**
     * 料批id
     */
    @ApiModelProperty("料批id")
    private Integer materialBatchId;
    /**
     * 料批名称
     */
    @ApiModelProperty("料批名称")
    private String materialBatchName;
    /**
     * 炉号
     */
    @ApiModelProperty("炉号")
    private String furnaceNumber;
    /**
     * 炉号id
     */
    @ApiModelProperty("炉号id")
    private Integer furnaceNumberId;
    /**
     * 班次
     */
    @ApiModelProperty("班次")
    private String classes;
    /**
     * 备注
     */
    @ApiModelProperty("备注")
    private String remark;
    /**
     * 机械生产时间
     */
    @ApiModelProperty("机械生产时间")
    private String machineProductionTime;
    /**
     * 电炉生产时间
     */
    @ApiModelProperty("电炉生产时间")
    private String electricFurnaceProductionTime;
    /**
     * 有功电量
     */
    @ApiModelProperty("有功电量")
    private BigDecimal activeElectricity;
    /**
     * 无功电量
     */
    @ApiModelProperty("无功电量")
    private BigDecimal kvarh;
    /**
     * 确认状态(0已保存，1已确认)
     */
    @ApiModelProperty("确认状态(0已保存，1已确认)")
    private Integer state;

    @ApiModelProperty("料出库明细返回信息")
    List<ErpOutOfStockOrderDetailDTO> detailDTOList;




}
