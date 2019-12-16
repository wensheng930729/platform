package com.bee.platform.user.authority.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * @author liang.li
 * @ClassName AuthEnterpriseAddRQ
 * @Description auth企业查询rq
 * @Date 2019-5-20
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("新权限后台企业查询rq")
public class AuthEnterpriseAllGetRQ {

    @ApiModelProperty(value = "公司名称")
    private String  name;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @ApiModelProperty(value = "开始时间")
    private Date start;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @ApiModelProperty(value = "结束时间")
    private Date end;


}
