package com.bee.platform.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
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
@ApiModel("成品入库请求参数")
public class ErpWarehousingOrderRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id 入库单号-入库日期-公司-料批-化验单-炉号-班次-产成品-计量单位-仓库-入库数量-备注
     */
    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("成品入库编号")
    @NotNull(message = "成品入库编号不能为空")
    private String code;

    @ApiModelProperty("入库日期")
    @NotNull(message = "入库日期不能为空")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date warehousingTime;

    @ApiModelProperty("公司id")
    @NotNull(message = "公司id不能为空")
    private Integer companyId;

    @ApiModelProperty("料批id")
    @NotNull(message = "料批id不能为空")
    private Integer materialBatchId;

    @ApiModelProperty("化验单id")
    @NotNull(message = "化验单id不能为空")
    private Integer testReportId;


    @ApiModelProperty("炉号")
    @NotNull(message = "炉号不能为空")
    private Integer furnaceId;

    @ApiModelProperty("班次")
    @NotNull(message = "班次不能为空")
    private String classes;

    @ApiModelProperty("产成品id")
    @NotNull(message = "产成品id不能为空")
    private Integer productId;

    @ApiModelProperty("产成品批次id")
    private Integer productBatchId;


    @ApiModelProperty("单位")
    @NotNull(message = "单位不能为空")
    private String unit;

    @ApiModelProperty("仓库id")
    @NotNull(message = "仓库id")
    private Integer repositoryId;

    @ApiModelProperty("入库数量")
    @NotNull(message = "入库数量不能为空")
    @Min(value = 0,message = "入库数量不能小于0")
    private BigDecimal amount;

    @ApiModelProperty("备注")
    @Length(max = 200,message = "备注不超过200字")
    private String remark;


    @ApiModelProperty("机械生产时间")
    @NotNull(message = "机械生产时间不能为空")
    private String machineProductionTime;

    @ApiModelProperty("电炉生产时间")
    @NotNull(message = "电炉生产时间不能为空")
    private String electricFurnaceProductionTime;

    @ApiModelProperty("有功电量")
    @NotNull(message = "有功电量不能为空")
    private BigDecimal activeElectricity;

    @ApiModelProperty("无功电量")
    @NotNull(message = "无功电量不能为空")
    private BigDecimal kvarh;


    @ApiModelProperty("确认状态(0已保存，1已确认)")
    private Integer state;
}
