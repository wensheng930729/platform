package com.bee.platform.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @ClassName TestReportDetailRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/28$ 13:56$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("化验单保存详细请求信息")
public class TestReportDetailRQ implements Serializable {

    private static final long serialVersionUID = -7037736674049880279L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("编号")
    private String code;

    @ApiModelProperty("公司id")
    @NotNull(message = "公司id")
    private Integer company;

    @ApiModelProperty("公司名称")
    @NotEmpty(message = "销售订单号不能为空")
    private String companyName;

    @ApiModelProperty("产品id")
    private Integer product;

    @ApiModelProperty("产品批次id")
    private Integer batchId;

    /*@ApiModelProperty("产品批次名称")
    private String batchName;*/

    @ApiModelProperty("化验类型id")
    private Integer testType;

    @ApiModelProperty("化验类型类别(0期初 1采购 2销售 3生产)")
    private Integer testTypeCategory;

    @ApiModelProperty("关联单号")
    private Integer orderNo;

    @ApiModelProperty("关联单号编号")
    private String orderCode;

    @ApiModelProperty("单据类别：采购，生产，销售")
    private String orderType;

    @ApiModelProperty("客户id")
    private Integer customer;

    @ApiModelProperty("客户名称")
    private String customerName;

    @ApiModelProperty("炉号")
    private String boilerId;

    @ApiModelProperty("班次")
    private String shiftsId;

    @ApiModelProperty("化验人")
    private String testUser;

    @ApiModelProperty("化验日期")
    @JsonFormat(pattern = "yyyy-MM-dd hh:mm:ss")
    @NotNull(message = "化验日期不能为空")
    private Date testDate;

    @JsonFormat(pattern = "yyyy-MM-dd")
    @ApiModelProperty("生产日期")
    private Date productDate;

    @ApiModelProperty("化验结果，存储json")
    //private String result;
    private List<ErpTestReportDetailRQ> detailList;

    @ApiModelProperty("化验单状态（0不合格 1合格）")
    private Integer state;
}
