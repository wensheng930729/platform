package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @ClassName ErpTestReportDTO
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/6/6$ 11:40$
 * @version 1.0.0
 */

@Data
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("化验单列表对象")
@JsonInclude(JsonInclude.Include.ALWAYS)
public class ErpTestReportDTO implements Serializable {

    private static final long serialVersionUID = 2708325555826364889L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("编号")
    private String code;

    @ApiModelProperty("化验类型id")
    private Integer testType;

    @ApiModelProperty("化验类型类别(1采购 2销售 3生产)")
    private Integer testTypeCategory;

    @ApiModelProperty("化验类型名称")
    private String testTypeName;

    @ApiModelProperty("产品id")
    private Integer product;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("单据类别：采购，生产，销售，")
    private String orderType;

    @ApiModelProperty("公司id")
    private Integer company;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("关联单号id")
    private Integer orderNo;

    @ApiModelProperty("关联单号编号")
    private String orderCode;

    @ApiModelProperty("客户id")
    private Integer customer;

    @ApiModelProperty("客户名称")
    private String customerName;

    @ApiModelProperty("化验人")
    private String testUser;

    @ApiModelProperty("化验日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date testDate;

    @ApiModelProperty("生产日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date productDate;

    @ApiModelProperty("化验结果")
    private String result;

    @ApiModelProperty("炉号")
    private String boilerId;

    @ApiModelProperty("班次")
    private String shiftsId;

    @ApiModelProperty("化验项目")
    //private String result;
    private List<ErpTestReportDetailDTO> detailDTO;

    @ApiModelProperty("化验单状态(0不合格 1合格)")
    private Integer state;
}
