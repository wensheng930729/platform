package com.bee.platform.common.utils;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.bee.platform.common.config.AuthRestTemplateConfig;
import com.bee.platform.common.config.property.CommonAuthConfigProperties;
import com.bee.platform.common.dto.AuthEnterpriseFlatDTO;
import com.google.common.collect.Lists;

import lombok.extern.slf4j.Slf4j;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName EnterpriseInfoUtils
 * @Description 功能描述
 * @Date 2019/6/1 16:17
 **/

@Slf4j
@Component
public class EnterpriseInfoUtils {

    public static final String GET_ENTERPRISE_ID="/authEnterprise/user/flat";

    @Autowired
    private AuthRestTemplateConfig restTemplateConfig;

    @Autowired
    CommonAuthConfigProperties props;

    public   List<Integer> getEnterpriseIds(String sysToken) {
        List<Integer>  enterpriseIds = Lists.newArrayList();
        String url=props.getExpectedAuthAddress()+GET_ENTERPRISE_ID+"?sysToken="+sysToken;
        HttpHeaders headers = setInnerCallHeader(MediaType.APPLICATION_FORM_URLENCODED);
        JSONObject jsonObject=restTemplateConfig.sendRestGet(url,headers);
        if(jsonObject==null){
            return enterpriseIds;
        }
        JSONArray array=jsonObject.getJSONArray("object");
        if(array==null){
            return enterpriseIds;
        }
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOList = array.toJavaList(AuthEnterpriseFlatDTO.class);
        if(!CollectionUtils.isEmpty(enterpriseFlatDTOList)){
             enterpriseIds = enterpriseFlatDTOList.stream().map(AuthEnterpriseFlatDTO::getValue).collect(Collectors.toList());
        }
        return enterpriseIds;

    }

    /**
     * 设置内部调用头信息
     * @param mt
     * @return
     */
    private HttpHeaders setInnerCallHeader(MediaType mt) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(mt);
        headers.add(AuthConstantsUtil.INNER_CLIENT_ID, AuthConstantsUtil.PLATFORM);
        return headers;
    }
}
