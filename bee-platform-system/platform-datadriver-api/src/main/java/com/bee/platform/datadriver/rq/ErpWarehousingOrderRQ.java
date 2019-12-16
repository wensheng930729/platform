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
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 成品入库编号
     */
    @ApiModelProperty("成品入库编号")
    @NotNull(message = "成品入库编号不能为空")
    private String code;
    /**
     * 入库日期
     */
    @ApiModelProperty("入库日期")
    @NotNull(message = "入库日期不能为空")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date warehousingTime;
    /**
     * 料批id
     */
    @ApiModelProperty("料批id")
    @NotNull(message = "料批id不能为空")
    private Integer materialBatchId;
    /**
     * 料批名称
     */
    @ApiModelProperty("料批名称")
//    @NotNull(message = "料批名称不能为空")
    private String materialBatchName;

    @ApiModelProperty("炉号")
    @NotNull(message = "炉号不能为空")
    private Integer furnaceId;

    /**
     * 班次
     */
    @ApiModelProperty("班次")
    @NotNull(message = "班次不能为空")
    private String classes;
    /**
     * 备注
     */
    @ApiModelProperty("备注")
    @Length(max = 200,message = "备注不超过200字")
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
    @NotNull(message = "产成品id不能为空")
    private Integer productId;

    /**
     * 产成品名称
     */
    @ApiModelProperty("产成品名称")
//    @NotNull(message = "产成品名称不能为空")
    private String productName;

    @ApiModelProperty("仓库id")
    @NotNull(message = "仓库id")
    private Integer repositoryId;
    /**
     * 仓库名称
     */
    @ApiModelProperty("仓库名称")
//    @NotNull(message = "仓库名称不能为空")
    private String storeHouseName;
    /**
     * 单位
     */
    @ApiModelProperty("单位")
    @NotNull(message = "单位不能为空")
    private String unit;
    /**
     * 入库数量
     */
    @ApiModelProperty("入库数量")
    @NotNull(message = "入库数量不能为空")
    @Min(value = 0,message = "入库数量不能小于0")
    private BigDecimal amount;
    /**
     * 公司id
     */
    @ApiModelProperty("公司id")
    @NotNull(message = "公司id不能为空")
    private Integer companyId;
    /**
     * 公司名称
     */
    @ApiModelProperty("公司名称")
//    @NotNull(message = "公司名称不能为空")
    private String companyName;

    /**
     * 化验单id
     */
    @ApiModelProperty("化验单id")
//    @NotNull(message = "化验单id不能为空")
    private Integer testReportId;
    /**
     * 化验单编号
     */
    @ApiModelProperty("化验单编号")
//    @NotNull(message = "化验单编号不能为空")
    private String testReportCode;


}
