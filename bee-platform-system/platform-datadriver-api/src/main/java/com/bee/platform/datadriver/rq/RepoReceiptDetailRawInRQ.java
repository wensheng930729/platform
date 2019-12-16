package com.bee.platform.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @Classname RepoReceiptDetail
 * @Description 入库明细
 * @Date 2019/5/28 16:50
 * @Author xin.huang
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("原料入库明细请求参数")
public class RepoReceiptDetailRawInRQ implements Serializable {

    private static final long serialVersionUID = 4181264684522476304L;

    @ApiModelProperty("原料入库明细id")
    private Integer id;

    @ApiModelProperty("关联的原料入库单据id")
    @NotNull(message = "关联的原料入库单据id不能为空")
    private Integer receiptId;

    @ApiModelProperty("产品id")
    @NotNull(message = "产品id不能为空")
    private Integer productId;

    @ApiModelProperty("原料入库日期")
    @NotNull(message = "原料入库日期不能为空")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date recordTime;

    @ApiModelProperty("提单号")
    private String voucherNo;

    @ApiModelProperty("湿重")
    @NotNull(message = "湿重不能为空")
    private BigDecimal wetWeight;

    @ApiModelProperty("计量单位")
    @NotEmpty(message = "计量单位不能为空")
    private String unit;

    @ApiModelProperty("仓库id")
    @NotNull(message = "仓库id不能为空")
    private Integer repositoryId;

    @ApiModelProperty("仓库名称")
//    @NotNull(message = "仓库名称不能为空")
    private String repositoryName;

    @ApiModelProperty("化验单id")
    @NotNull(message = "化验单id不能为空")
    private Integer testId;

    @ApiModelProperty("水分率")
    private String waterRate;

    @ApiModelProperty("车牌号")
    private String plateNo;

    @ApiModelProperty("毛重")
    private BigDecimal roughWeight;

    @ApiModelProperty("皮重")
    private BigDecimal weight;

    @ApiModelProperty("品位")
    private BigDecimal grade;

    @ApiModelProperty("实收数量")
    @NotNull(message = "实收数量不能为空")
    @Min(value = 0,message = "原料入库数量不能小于0")
    private BigDecimal num;

    @ApiModelProperty("公司id")
    private Integer companyId;

}
