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
 * 成品入库主表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-30
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("成品入库返回信息")
@JsonInclude
public class ErpWarehousingOrderDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 成品入库编号
     */
    @ApiModelProperty("成品入库编号")
    private String code;
    /**
     * 入库日期
     */
    @ApiModelProperty("入库日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date warehousingTime;
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
    private String furnaceNumber; /**
     * 炉号
     */
    @ApiModelProperty("炉号id")
    private Integer furnaceId;
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
     * 确认状态(0已保存，1已确认)
     */
    @ApiModelProperty("确认状态(0已保存，1已确认)")
    private Integer state;
    /**
     * 附件名
     */
    @ApiModelProperty("附件名")
    private String fileName;
    /**
     * 附件url
     */
    @ApiModelProperty("附件url")
    private String fileUrl;

    /**
     * 产成品id
     */
    @ApiModelProperty("产成品id")
    private Integer productId;

    /**
     * 产成品名称
     */
    @ApiModelProperty("产成品名称")
    private String productName;
    /**
     * 产品批次id
     */
    @ApiModelProperty("产品批次id")
    private Integer productBatchId;

    @ApiModelProperty("产成品拼批次")
    private String productAndBatch;
    /**
     * 仓库id
     */
    @ApiModelProperty("仓库id")
    private Integer repositoryId;

    /**
     * 仓库名称
     */
    @ApiModelProperty("仓库名称")
    private String storeHouseName;
    /**
     * 单位
     */
    @ApiModelProperty("单位")
    private String unit;
    /**
     * 入库数量
     */
    @ApiModelProperty("入库数量")
    private BigDecimal amount;
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
     * 化验单id
     */
    @ApiModelProperty("化验单id")
    private Integer testReportId;
    /**
     * 化验单编号
     */
    @ApiModelProperty("化验单编号")
    private String testReportCode;

    @ApiModelProperty("机械生产时间")
    private String machineProductionTime;

    @ApiModelProperty("电炉生产时间")
    private String electricFurnaceProductionTime;

    @ApiModelProperty("有功电量")
    private BigDecimal activeElectricity;

    @ApiModelProperty("无功电量")
    private BigDecimal kvarh;

}
